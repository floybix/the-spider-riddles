(load-game-file "utils.clj")

(defn create-entity
  [img]
  (assoc img
         :width 2
         :height 2
         :x-velocity 0
         :y-velocity 0
         ))

(defn rectangle-from-object-layer
  [screen obj-name]
  (let [obj (-> (map-layer screen "entities")
                (map-objects)
                (.get obj-name))
        rect (.getRectangle obj)]
    {:id (keyword obj-name)
     :x (/ (.x rect) pixels-per-tile)
     :y (/ (.y rect) pixels-per-tile)
     :width  (/ (.width rect) pixels-per-tile)
     :height  (/ (.height rect) pixels-per-tile)}))

(defn create-entity-from-object-layer
  [screen obj-name]
  (let [obj (-> (map-layer screen "entities")
                (map-objects)
                (.get obj-name))
        img (-> (tiled-map! screen :get-tile-sets)
                 (.getTile (-> (.getProperties obj) (.get "gid")))
                 (.getTextureRegion)
                 (texture))
        rect (.getRectangle obj)]
    (assoc (create-entity img)
           :id (keyword obj-name)
           :x (/ (.x rect) pixels-per-tile)
           :y (/ (.y rect) pixels-per-tile)
           :width (/ (texture! img :get-region-width) pixels-per-tile)
           :height (/ (texture! img :get-region-height) pixels-per-tile))))

(defn create-character
  [walk-layers down up stand-left walk-left]
  (let [down-flip (texture down :flip true false)
        up-flip (texture up :flip true false)
        stand-flip (texture stand-left :flip true false)
        walk-flip (texture walk-left :flip true false)
        ]
    (assoc (create-entity down)
           :character? true
           :walk-layers walk-layers
           :direction :down
           :down (animation duration [down down-flip])
           :up (animation duration [up up-flip])
           :left (animation duration [stand-left walk-left])
           :right (animation duration [stand-flip walk-flip]))))

(defn create-spider
  [screen obj-name]
  (let [obj-info (create-entity-from-object-layer screen obj-name)
        down (texture "spider-front.png")]
    (-> (create-character #{"path"} down down down down)
        (assoc :spider? true
               :hurt-sound (sound "enemy_hurt.wav"))
        (merge (select-keys obj-info [:id :x :y])))))

(defn create-shark
  [screen obj-name]
  (let [obj-info (create-entity-from-object-layer screen obj-name)
        fin (texture "shark-fin.png")
        attack (texture "shark-front-bite.png")]
    (-> (create-character float-layers fin fin fin fin)
        (assoc :shark? true
               :angle 0
               :up nil
               :down nil
               :left nil
               :right nil
               :fin fin
               :attack attack
               :hurt-sound (sound "enemy_hurt.wav"))
        (merge (select-keys obj-info [:id :x :y])))))

(defn create-player
  []
  (let [down (texture "elf-front.png")
        up (texture "elf-back.png")
        stand-left (texture "elf-left-walk.png" :set-region 0 0 64 64)
        walk-left (texture "elf-left-walk.png" :set-region 64 0 64 64)
        float (texture "elf-front-floaty.png")]
    (assoc (create-character player-layers down up stand-left walk-left)
           :player? true
           :id :player
           :health 100
           :riddles-done #{}
           :float float
           :hurt-sound (sound "player_hurt.wav")
           :death-sound (sound "player_death.wav"))))

(defn create-eruption
  [screen obj-name]
  (merge (particle-effect "fire.p" :scale-effect 0.02)
         (rectangle-from-object-layer screen obj-name)))

(defn move
  [screen entity]
  (let [[x-velocity y-velocity] (if (:player? entity)
                                  (get-player-velocity entity)
                                  [(:x-velocity entity 0)
                                   (:y-velocity entity 0)])
        x-change (* x-velocity (:delta-time screen))
        y-change (* y-velocity (:delta-time screen))]
    (if (or (not= 0 x-change) (not= 0 y-change))
      (assoc entity
             :x-velocity (decelerate x-velocity)
             :y-velocity (decelerate y-velocity)
             :x-change x-change
             :y-change y-change
             :x (+ (:x entity) x-change)
             :y (+ (:y entity) y-change))
      entity)))

(defn animate
  [screen entity]
  (cond
    (:jumping? entity)
    (if-let [add (first (:jump-seq entity))]
      (-> entity
        (update-in [:width] + add)
        (update-in [:height] + add)
        (update-in [:x] - (/ add 2))
        (update-in [:y] - (/ add 2))
        (update-in [:jump-seq] next))
      ;; end of jump
      (assoc entity :jumping? nil :jump-seq nil)
      )
    (:floating? entity)
    (merge entity (:float entity))
    (:shark? entity)
    (let [angle (Math/atan2 (:x-change entity) (:y-change entity))]
      (assoc entity :angle angle))
    :else
    (if-let [direction (get-direction entity)]
      (if-let [anim (get entity direction)]
        (merge entity
               (animation->texture screen anim)
               {:direction direction})
        entity)
      entity)))

(defn start-jump
  [player]
  (if (:jumping? player)
    player
    (assoc player
         :jumping? true
         :jump-seq jump-add-seq)))

(defn not-victim?
  [attacker victim]
  (or (= (:health attacker) 0)
      (not= (:npc? attacker) (:player? victim))
      (not (near-entity? attacker victim attack-distance))
      (case (:direction attacker)
        :down (< (- (:y attacker) (:y victim)) 0) ; victim is up?
        :up (> (- (:y attacker) (:y victim)) 0) ; victim is down?
        :right (> (- (:x attacker) (:x victim)) 0) ; victim is left?
        :left (< (- (:x attacker) (:x victim)) 0) ; victim is right?
        false)))

(defn attack
  [entities attacker]
  (let [victim (first (drop-while #(not-victim? attacker %) entities))]
    (map (fn [e]
           (if (= e victim)
             (let [health (max 0 (- (:health e) (:damage attacker)))]
               (assoc e
                      :play-sound (if (and (= health 0) (:death-sound victim))
                                    (:death-sound victim)
                                    (:hurt-sound victim))
                      :health health))
             e))
         entities)))

(defn npc-attacker?
  [entity player]
  (and player
       (:npc? entity)
       (> (:health entity) 0)
       (= (:attack-time entity) 0)
       (near-entity? entity player attack-distance)))

(defn attack-player
  [entities]
  (if-let [npc (find-first #(npc-attacker? % (find-first :player? entities))
                           entities)]
    (attack entities npc)
    entities))

(defn prevent-move
  [screen entities entity]
  (if-not (:character? entity)
    entity
    (if (or (= (:health entity) 0)
          (< (:x entity) 0)
          (> (:x entity) (- map-width 1))
          (< (:y entity) 0)
          (> (:y entity) (- map-height 1))
          (not (some #(on-layer-ok? screen entity %)
                     (:walk-layers entity))))
    (assoc entity
           :x-velocity 0
           :y-velocity 0
           :x-change 0
           :y-change 0
           :x (- (:x entity) (:x-change entity))
           :y (- (:y entity) (:y-change entity)))
    entity)))

(defn adjust-times
  [screen entity]
  (if-not (:character? entity)
    entity
    (if-let [attack-time (:attack-time entity)]
      (assoc entity
           :attack-time
           (if (> attack-time 0)
             (max 0 (- attack-time (:delta-time screen)))
             max-attack-time))
    entity)))
