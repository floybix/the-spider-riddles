(load-game-file "entities.clj")

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
  (fn [screen _]
    (let [renderer (orthogonal-tiled-map "level.tmx" (/ 1 pixels-per-tile))
          screen (update! screen :camera (orthographic) :renderer renderer)
          player (assoc (create-player)
                        :id :player
                        :walk-layers #{"path" "bridges"}
                        :x 5 :y (- (dec map-height) 24))
          spiders [(assoc (create-spider screen "spider-1")
                          :walk-layers #{"pits"})
                   (create-spider screen "spider-2")]
          rope (create-entity-from-object-layer screen "rope")
          ;sword
          ]
      (concat [player rope]
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
         ;(attack-player)
         (play-sounds!)
         ;(remove #(<= (:health %) 0))
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
        ;(attack entities player)
        )))
  
  :on-touch-down
  (fn [screen entities]
    (when-let [player (find-first :player? entities)]
      (let [min-x (/ (game :width) 3)
            max-x (* (game :width) (/ 2 3))
            min-y (/ (game :height) 3)
            max-y (* (game :height) (/ 2 3))]
        (when (and (< min-x (game :x) max-x)
                   (< min-y (game :y) max-y))
          ;(attack entities player)
          )))))

(defscreen status-screen
  :on-show
  (fn [screen _]
    (update! screen :camera (orthographic) :renderer (stage))
    [(assoc (label "0" (color :white))
           :id :health
           :x 5)]
    )
  
  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :health (doto entity (label! :set-text (str (:health @player-data))))
             entity))
         (render! screen)))
  
  :on-resize
  (fn [screen entities]
    (height! screen 300)))

(defscreen spider-screen
  :on-show
  (fn [screen _]
    (update! screen :camera (orthographic) :renderer (stage))
    []
    )
  
  :on-render
  (fn [screen entities]
    (clear!)
    (->> (for [entity entities]
           (case (:id entity)
             entity))
         (render! screen)))
  
  :on-resize
  (fn [screen entities]
    (height! screen 300))
  
  :on-touch-down
  (fn [screen entities]
    (println (game :x) (game :y))
    )
  )

(set-game-screen! main-screen status-screen)
