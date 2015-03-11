(def vertical-tiles 20)
(def pixels-per-tile 32)
(def duration 0.2)
(def damping 0.5)
(def max-velocity 6)
(def max-velocity-npc 3)
(def deceleration 0.9)
(def map-width 90)
(def map-height 70)
(def max-attack-time 1)
(def aggro-distance 6)
(def attack-distance 1.5)

(def soft-layers #{"path" "bridges"})
;; note: other layers are hard, e.g. pits

(defn touching-layer?
  [screen entity layer-name]
  (let [layer (tiled-map-layer screen layer-name)]
    (->> (for [tile-x (range (int (:x entity))
                             (+ (:x entity) (:width entity)))
               tile-y (range (int (:y entity))
                             (+ (:y entity) (:height entity)))]
           (tiled-map-cell layer tile-x tile-y))
         (some boolean))))
 
(defn all-on-layer?
  [screen entity layer-name]
  (let [layer (tiled-map-layer screen layer-name)]
    (->> (for [tile-x (range (int (:x entity))
                             (+ (:x entity) (:width entity)))
               tile-y (range (int (:y entity))
                             (+ (:y entity) (:height entity)))]
           (tiled-map-cell layer tile-x tile-y))
         (every? boolean))))

(defn centre-on-layer?
  [screen entity layer-name pad]
  (let [layer (tiled-map-layer screen layer-name)
        mid-x (+ (:x entity) (/ (:width entity) 2))
        mid-y (+ (:y entity) (/ (:height entity) 2))]
    (->> (for [off-x [(- pad) pad]
               off-y [(- pad) pad]]
           (tiled-map-cell layer (int (+ mid-x off-x))
                                 (int (+ mid-y off-y))))
         (every? boolean))))

(defn on-layer-ok?
  [screen entity layer-name]
  (if (soft-layers layer-name)
    (centre-on-layer? screen entity layer-name 0.5)
    (centre-on-layer? screen entity layer-name 1.0)))

(defn near-entity?
  [e e2 min-distance]
  (and (not= (:id e) (:id e2))
       (nil? (:draw-time e2))
       (< (Math/abs ^double (- (:x e) (:x e2))) min-distance)
       (< (Math/abs ^double (- (:y e) (:y e2))) min-distance)))

(defn near-entities?
  [entities entity min-distance]
  (some #(near-entity? entity % min-distance) entities))

(defn decelerate
  [velocity]
  (let [velocity (* velocity deceleration)]
    (if (< (Math/abs velocity) damping)
      0
      velocity)))

(defn touched?
  [key]
  (and (game :touched?)
       (case key
         :down (< (game :y) (/ (game :height) 3))
         :up (> (game :y) (* (game :height) (/ 2 3)))
         :left (< (game :x) (/ (game :width) 3))
         :right (> (game :x) (* (game :width) (/ 2 3)))
         false)))

(defn get-player-velocity
  [entity]
  [(cond
     (or (key-pressed? :dpad-left) (touched? :left)) (* -1 max-velocity)
     (or (key-pressed? :dpad-right) (touched? :right)) max-velocity
     :else (:x-velocity entity))
   (cond
     (or (key-pressed? :dpad-down) (touched? :down)) (* -1 max-velocity)
     (or (key-pressed? :dpad-up) (touched? :up)) max-velocity
     :else (:y-velocity entity))])

(defn get-npc-axis-velocity
  [diff]
  (cond
    (> diff attack-distance) (* -1 max-velocity-npc)
    (< diff (* -1 attack-distance)) max-velocity-npc
    :else 0))

(defn get-npc-aggro-velocity
  [npc player]
  (let [x-diff (- (:x npc) (:x player))
        y-diff (- (:y npc) (:y player))]
    [(get-npc-axis-velocity x-diff)
     (get-npc-axis-velocity y-diff)]))

(defn get-npc-velocity
  [entities entity]
  (let [player (find-first :player? entities)]
    (if (and player (near-entity? entity player aggro-distance))
      (get-npc-aggro-velocity entity player)
      (if (= (:attack-time entity) 0)
        [(* max-velocity-npc (- (rand-int 3) 1))
         (* max-velocity-npc (- (rand-int 3) 1))]
        [(:x-velocity entity) (:y-velocity entity)]))))

(defn get-velocity
  [entities entity]
  (cond
    (:player? entity) (get-player-velocity entity)
    (:npc? entity) (get-npc-velocity entities entity)
    :else [0 0]))

(defn get-direction
  [entity]
  (cond
    (not= (:y-velocity entity) 0) (if (> (:y-velocity entity) 0) :up :down)
    (not= (:x-velocity entity) 0) (if (> (:x-velocity entity) 0) :right :left)
    :else nil))

(defn x-centered
  [entity]
  (let [{:keys [x width]} entity]
    (assoc entity
           :x (+ x (/ width 2)))))

(defn play-sounds!
  [entities]
  (doseq [{:keys [play-sound]} entities]
    (when play-sound
      (sound! play-sound :play)))
  (map #(dissoc % :play-sound) entities))

(defn render-map-fixed!
  [{:keys [^com.badlogic.gdx.maps.tiled.renderers.BatchTiledMapRenderer renderer ^Camera camera] :as screen}
   & [k & layer-names]]
  (when camera (.setView renderer camera))
  (if k
    (let [all-layer-names (map-layer-names screen)]
      ; make sure the layer names exist
      (doseq [n layer-names]
        (when-not (contains? (set all-layer-names) n)
          (throw (Exception. (format "Layer \"%s\" does not exist." n)))))
      ; render with or without the supplied layers
      (->> (case k
             :with (set layer-names)
             :without (clojure.set/difference (set all-layer-names)
                                              (set layer-names))
             #_(u/throw-key-not-found k))
           (map #(.indexOf ^java.util.List all-layer-names %))
           (sort)
           int-array
           (.render renderer))))
  nil)
