(def vertical-tiles 20)
(def pixels-per-tile 32)
(def duration 0.2)
(def damping 0.5)
(def max-velocity 6)
(def deceleration 0.9)
(def map-width 90)
(def map-height 70)

(def player-layers #{"path" "bridges" "lava"})
(def float-layers #{"Shark pool" "lake of terror"})

(def hard-layers #{"pits" "Shark pool" "lake of terror"})
;; note: other layers are soft

(def jump-add-seq (concat (repeat 20 0.06) (repeat 20 -0.06)))

(defn abs [x] (if (neg? x) (- x) x))

(defn touching-layer?
  [screen entity layer-name]
  (let [layer (tiled-map-layer screen layer-name)]
    (->> (for [tile-x (range (int (:x entity))
                             (+ (:x entity) (:width entity)))
               tile-y (range (int (:y entity))
                             (+ (:y entity) (:height entity)))]
           (tiled-map-cell layer tile-x tile-y))
         (some boolean))))
 
(defn central-points-on-layer
  "Returns booleans in sequence: ([-x -y] [-x +y] [+x -y] [+x +y])."
  [screen entity layer-name pad]
  (let [layer (tiled-map-layer screen layer-name)
        mid-x (+ (:x entity) (/ (:width entity) 2))
        mid-y (+ (:y entity) (/ (:height entity) 2))]
    (for [off-x [(- pad) pad]
          off-y [(- pad) pad]]
      (tiled-map-cell layer (int (+ mid-x off-x))
                      (int (+ mid-y off-y))))))

(defn test-points-on-layer
  [screen entity layer-name]
  (if (hard-layers layer-name)
    (central-points-on-layer screen entity layer-name 1.0)
    (central-points-on-layer screen entity layer-name 0.5)))

(defn on-layer-ok?
  [screen entity layer-name]
  (every? boolean (test-points-on-layer screen entity layer-name)))

(defn on-layer-xy-ok
  "Returns which dimensions are ok: [:x] [:y] [:x :y] or []."
  [screen entity layer-name]
  (let [checks (test-points-on-layer screen entity layer-name)]
    (case (mapv boolean checks)
      ;; sw nw se ne
      [true true false false] [:y]
      [false false true true] [:y]
      [true false true false] [:x]
      [false true false true] [:x]
      (if (every? boolean checks)
        [:x :y]
        []))))

(defn near-entity?
  [e e2 min-distance]
  (and (not= (:id e) (:id e2))
       (< (abs (- (:x e) (:x e2))) min-distance)
       (< (abs (- (:y e) (:y e2))) min-distance)))

(defn near-entities?
  [entities entity min-distance]
  (some #(near-entity? entity % min-distance) entities))

(defn go-round-and-round
  [entity]
  (let [[fx fy] (:focus-point entity)
        x (:x entity)
        y (:y entity)]
    (if (or (> (abs (- fx x)) 3.5)
            (> (abs (- fy y)) 3.5))
      ;; stuck
      (assoc entity
        :x-velocity (- fx x)
        :y-velocity (- fy y))
      ;; ok
      (assoc entity
        :x-velocity (- y fy)
        :y-velocity (- fx x)
       ))))

(defn chase
  [entity player]
  (assoc entity
    :x-velocity (-> (- (:x player) (:x entity))
                    (min max-velocity)
                    (max (- max-velocity)))
    :y-velocity (-> (- (:y player) (:y entity))
                    (min max-velocity)
                    (max (- max-velocity)))))

(defn slow-down
  [entity]
 (assoc entity
        :x-velocity (/ (:x-velocity entity) 2)
        :y-velocity (/ (:y-velocity entity) 2)))
 
  
  
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
         :up (> (game :y) (* (game :height) 0.6))
         :down (and (< (game :y) (* (game :height) 0.4))
                    ;; avoid buttons
                    (> (game :x) (* (game :width) 0.2)))
         :left (and (< (game :x) (* (game :width) 0.4))
                    ;; avoid buttons
                    (> (game :x) (* (game :width) 0.2)))
         :right (> (game :x) (* (game :width) 0.6))
         false)))

(defn get-player-velocities
  [entity]
  {:x-velocity (cond
                (or (key-pressed? :dpad-left) (touched? :left)) (* -1 max-velocity)
                (or (key-pressed? :dpad-right) (touched? :right)) max-velocity
                :else (:x-velocity entity))
   :y-velocity (cond
                (or (key-pressed? :dpad-down) (touched? :down)) (* -1 max-velocity)
                (or (key-pressed? :dpad-up) (touched? :up)) max-velocity
                :else (:y-velocity entity))})

(defn get-direction
  [entity]
  (cond
    (not= (:y-velocity entity 0) 0) (if (> (:y-velocity entity) 0) :up :down)
    (not= (:x-velocity entity 0) 0) (if (> (:x-velocity entity) 0) :right :left)
    :else nil))

(defn x-centered
  [entity]
  (let [{:keys [x width]} entity]
    (assoc entity
           :x (+ x (/ width 2)))))

(defn find-by-id
  [id entities]
  (find-first #(= id (:id %)) entities))

(defn update-by-id
  [entities id f & args]
  (for [e entities]
    (if (= id (:id e))
      (apply f e args)
      e)))

(defn play-sounds!
  [entities]
  (doseq [{:keys [play-sound]} entities]
    (when play-sound
      (sound! play-sound :play)))
  (map #(dissoc % :play-sound) entities))

