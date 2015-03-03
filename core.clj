(load-game-file "entities.clj")

(declare main-screen status-screen)

;; one of :map, :spider
(def current-screen (atom :map))

(defn camera-follow-player!
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

(defn pick-up-items
  [entities]
  (when-let [player (find-first :player? entities)]
    (->>
      (for [item entities]
        (if (and (near-entity? player item 1)
                 (not (:character item)))
          ;; pick it up
          (do (screen! status-screen :on-pick-up-item :which-entity item)
            nil)
          ;; otherwise - ignore it
          item))
      (remove nil?))))

(defscreen main-screen
  :on-show
  (fn [screen _]
    (let [renderer (orthogonal-tiled-map "level.tmx" (/ 1 pixels-per-tile))
          screen (update! screen :camera (orthographic) :renderer renderer)
          player (assoc (create-player)
                        :walk-layers #{"path" "bridges"}
                        :x 5 :y (- (dec map-height) 24))
          spiders [(assoc (create-spider screen "spider-1")
                          :walk-layers #{"pits"})
                   (create-spider screen "spider-2")]
          rope (create-entity-from-object-layer screen "rope")
          sword (create-entity-from-object-layer screen "sword")
          ]
      (concat [player rope sword]
              spiders)))
  
  :on-render
  (fn [screen entities]
    (when (= :map @current-screen)
      (clear!)
      (->> entities
        (map (fn [entity]
               (->> entity
                 (move screen entities)
                 (animate screen)
                 (prevent-move screen entities))))
        (pick-up-items)
        (play-sounds!)
        (sort-by :y >)
        (render! screen)
        (camera-follow-player! screen))))
  
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

;;;; status screen - items, health

(def button->item-id (atom {}))
(def item-id->table-cell (atom {}))

(defscreen status-screen
  :on-show
  (fn [screen _]
    (update! screen :camera (orthographic) :renderer (stage))
    (height! screen 600)
    [(assoc (vertical [] :left :reverse)
            :item-table? true
            :id :item-table)
     (assoc (label "0" (color :white))
           :id :health
           :x 5 :y (- (height screen) 50))]
    )
  
  ;; when calling this, give :which-entity with the entity
  :on-pick-up-item
  (fn [screen entities]
    (let [e (-> (:which-entity screen)
              (update-in [:width] * pixels-per-tile)
              (update-in [:height] * pixels-per-tile))
          button (text-button "Use" (style :text-button nil nil nil (bitmap-font)))
          item-table (find-first :item-table? entities)
          held-e (horizontal [(image e) button])
          ]
      (swap! button->item-id assoc (:object button) (:id e))
      (swap! item-id->table-cell assoc (:id e) (:object held-e))
      (add! item-table held-e)
      entities))
  
  :on-ui-changed
  (fn [screen entities]
    (let [item-id (get @button->item-id (:actor screen))]
      (println item-id)
      )
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
    (height! screen 600)))

;;;; spider screen - sub-game for riddles, fighting

(defscreen spider-screen
  :on-show
  (fn [screen _]
    (update! screen :camera (orthographic) :renderer (stage))
    []
    )
  
  :on-render
  (fn [screen entities]
    (when (= :spider @current-screen)
      (clear!)
      (->> (for [entity entities]
             (case (:id entity)
               entity))
        (render! screen))))
  
  :on-resize
  (fn [screen entities]
    (height! screen 600))
  
  :on-touch-down
  (fn [screen entities]
    (println (game :x) (game :y))
    )
  )

(set-game-screen! main-screen spider-screen status-screen)
