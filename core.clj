(load-game-file "entities.clj")

(print "Press the arrow keys to move and the space bar to attack.")

(defn update-screen!
  [screen entities]
  (when-let [player (find-first :player? entities)]
    (position! screen (:x player) (:y player)))
  entities)

(defn play-sounds!
  [entities]
  (doseq [{:keys [play-sound]} entities]
    (when play-sound
      (sound! play-sound :play)))
  (map #(dissoc % :play-sound) entities))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [renderer (orthogonal-tiled-map "level.tmx" (/ 1 pixels-per-tile))
          screen (update! screen :camera (orthographic) :renderer renderer)
          player (assoc (create-player) :id 0 :x 5 :y (- (dec map-height) 24))
          spiders [(assoc (create-spider) :id 1 :x 12 :y (- (dec map-height) 27)
                          :start-layer "pits")
                   (assoc (create-spider) :id 2 :x 27 :y (- (dec map-height) 4))]]
      (concat [player]
              spiders)))
  
  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (map (fn [entity]
                (->> entity
                     (move screen entities)
                     (animate screen)
                     (prevent-move screen entities)
                     (adjust-times screen))))
         (attack-player)
         (play-sounds!)
         (remove #(<= (:health %) 0))
         (sort-by :y #(compare %2 %1))
         (render! screen)
         (update-screen! screen)))
  
  :on-resize
  (fn [screen entities]
    (height! screen vertical-tiles))
  
  :on-key-down
  (fn [screen entities]
    (when-let [player (find-first :player? entities)]
      (when (= (:key screen) (key-code :space))
        (print " ")
        (attack entities player))))
  
  :on-touch-down
  (fn [screen entities]
    (when-let [player (find-first :player? entities)]
      (let [min-x (/ (game :width) 3)
            max-x (* (game :width) (/ 2 3))
            min-y (/ (game :height) 3)
            max-y (* (game :height) (/ 2 3))]
        (when (and (< min-x (game :x) max-x)
                   (< min-y (game :y) max-y))
          (attack entities player))))))

(set-game-screen! main-screen)
