(load-game-file "utils.clj")

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
    (assoc img
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
    (assoc down
           :character? true
           :walk-layers walk-layers
           :width 2
           :height 2
           :x-velocity 0
           :y-velocity 0
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

(defn create-volcano
  [screen obj-name file-name]
  (-> (merge (particle-effect file-name :scale-effect 0.02)
             (rectangle-from-object-layer screen obj-name))
      (assoc :volcano? true)))

(defn move
  [screen entity]
  (let [[x-velocity y-velocity] [(:x-velocity entity 0)
                                 (:y-velocity entity 0)]
        x-change (* x-velocity (:delta-time screen))
        y-change (* y-velocity (:delta-time screen))]
    (if (or (not (zero? x-change)) (not (zero? y-change)))
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
    (if (:biting? entity)
      (-> (merge entity (:attack entity))
        (assoc :angle 0))
      (-> (merge entity (:fin entity))
        (assoc :angle (+ -90 (* 90 (/ (:x-velocity entity) max-velocity))))))
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

(defn prevent-move
  [screen entities entity]
  (if (or (not (:character? entity))
          (and (zero? (:x-change entity 0))
               (zero? (:y-change entity 0))))
    entity
    (let [xy-ok (->> (map #(on-layer-xy-ok screen entity %)
                          (:walk-layers entity))
                     (reduce into #{}))]
      (cond->
       entity
       (or (not (:y xy-ok))
           (< (:y entity) 0)
           (> (:y entity) (- map-height 1)))
       (assoc
         :y-velocity 0
         :y-change 0
         :y (- (:y entity) (:y-change entity)))
       (or (not (:x xy-ok))
           (< (:x entity) 0)
           (> (:x entity) (- map-width 1)))
       (assoc
         :x-velocity 0
         :x-change 0
         :x (- (:x entity) (:x-change entity)))
       ))))
