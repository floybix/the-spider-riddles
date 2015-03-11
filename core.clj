(load-game-file "entities.clj")

(declare main-screen status-screen spider-screen)

;; one of :main, :spider
(def current-screen-k (atom :main))

(defn narrate
  [text]
  (screen! status-screen :on-narration :say text))

(def start-story "You run out of the candy house and find yourself on a path through a grassy plain.")

;; main screen - the map

(defn camera-follow-player!
  [screen entities]
  (when-let [player (find-by-id :player entities)]
    (position! screen (:x player) (:y player)))
  entities)

(defn interact
  [entities]
  (when-let [player (find-by-id :player entities)]
    (->>
      (for [e entities]
        ;; something we can touch?
        (if (and (near-entity? player e 1)
                 (= (:in-pits? e) (:in-pits? player)))
          (cond
            ;; pick up items
            (:item? e)
            (do (screen! status-screen :on-pick-up-item :which-entity e)
              ;; nil to remove from screen
              nil)
            ;; meet spiders
            (:spider? e)
            (if (get (:riddles-done @player-data) (:id e))
              ;; already done this spider's riddle
              e
              (do (screen! spider-screen :on-enter :which-entity e)
                (reset! current-screen-k :spider)
                e))
            :else
            e)
          e))
      (remove nil?))))

(defn render-with-overlaps
  [screen entities]
  (render-map-fixed! screen :without "bridges")
  (draw! screen (filter :in-pits? entities))
  (render-map-fixed! screen :with "bridges")
  (draw! screen (->> (remove :in-pits? entities)
                  (map (fn [e] (if (particle-effect? e) (x-centered e) e)))))
  entities)

(defscreen main-screen
  :on-show
  (fn [screen _]
    ;(reset! current-screen-k :main)
    (let [renderer (orthogonal-tiled-map "level.tmx" (/ 1 pixels-per-tile))
          screen (update! screen :camera (orthographic) :renderer renderer)
          player (assoc (create-player)
                        :walk-layers #{"path" "bridges"}
                        :x 5 :y (- (dec map-height) 24))
          spiders [(assoc (create-spider screen "spider-1")
                          :walk-layers #{"pits"}
                          :in-pits? true
                          :riddle (str "What is one plus one on a snowy winter Monday in Argentina?")
                          :answer "two")
                   (create-spider screen "spider-2")]
          rope (assoc (create-entity-from-object-layer screen "rope")
                      :item? true
                      :in-pits? true)
          sword (assoc (create-entity-from-object-layer screen "sword")
                       :item? true)
          volc-1 (merge (particle-effect "fire.p" :scale-effect 0.02)
                        (rectangle-from-object-layer screen "volcano-1"))
          ]
      (add-timer! screen :eruption 5 5)
      (concat [player rope sword volc-1]
              spiders)))
  
  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :eruption (doseq [e entities]
                  (when (particle-effect? e)
                    (particle-effect! e :start)))))
  
  :on-render
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (clear!)
      (doseq [e entities]
        (when (particle-effect? e)
          (particle-effect! e :update (graphics! :get-delta-time))))
      ;;
      (->> entities
        (map (fn [entity]
               (->> entity
                 (move screen entities)
                 (animate screen)
                 (prevent-move screen entities))))
        (interact)
        (play-sounds!)
        (sort-by :y >)
        (render-with-overlaps screen)
        (camera-follow-player! screen))))
  
  :on-resize
  (fn [screen entities]
    (height! screen vertical-tiles))
  
  :on-use-item
  (fn [screen entities]
    (when-let [player (find-by-id :player entities)]
      (case (:item-id screen)
        :sword (if (on-layer-ok? screen player "bridges")
                 (->> (assoc player
                             :walk-layers #{"pits"}
                             :in-pits? true)
                   (conj (remove :player? entities))))
        :rope (if (on-layer-ok? screen player "bridges")
                (->> (assoc player
                            :walk-layers #{"path" "bridges"}
                            :in-pits? nil)
                  (conj (remove :player? entities))))
        )))
  
  :on-key-down
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (when (= (:key screen) (key-code :space))
          (->> (start-jump player)
            (conj (remove :player? entities)))))))
  
  :on-touch-down
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (let [min-x (/ (game :width) 3)
              max-x (* (game :width) (/ 2 3))
              min-y (/ (game :height) 3)
              max-y (* (game :height) (/ 2 3))]
          (when (and (< min-x (game :x) max-x)
                     (< min-y (game :y) max-y))
            ;(attack entities player)
            ))))))


;;;; status screen - items, health

(def button->item-id (atom {}))

(defscreen status-screen
  :on-show
  (fn [screen _]
    (update! screen :camera (orthographic) :renderer (stage))
    (height! screen 600)
    [(assoc (vertical [] :left :reverse)
            :id :item-table)
     (assoc (label "" (color :yellow))
           :id :health
           :x 10 :y (- (height screen) 80))
     (assoc (label start-story (color :white))
            :id :narration
            :x 10 :y (- (height screen) 50))]
    )
  
  ;; when calling this, give :which-entity with the entity
  :on-pick-up-item
  (fn [screen entities]
    (let [e (-> (:which-entity screen)
              (update-in [:width] * pixels-per-tile)
              (update-in [:height] * pixels-per-tile))
          ui-skin (skin "uiskin.json")
          button (text-button "Use" ui-skin)
          item-table (find-by-id :item-table entities)
          held-e (horizontal [(image e) button])
          ]
      (swap! button->item-id assoc (:object button) (:id e))
      (add! item-table held-e)
      entities))
  
  :on-ui-changed
  (fn [screen entities]
    (let [item-id (get @button->item-id (:actor screen))]
      (screen! main-screen :on-use-item :item-id item-id)
      entities))
  
  :on-narration
  (fn [screen entities]
    (for [entity entities]
      (if (= :narration (:id entity))
        (doto entity (label! :set-text (:say screen)))
        entity)))
  
  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :health (doto entity
                       (label! :set-text (str "health " (:health @player-data))))
             entity))
         (render! screen)))
  
  :on-resize
  (fn [screen entities]
    (height! screen 600)))


;;;; spider screen - sub-game for riddles, fighting

(defn make-speech-bubbles
  [screen]
  (let [spider-bubble (assoc (shape :filled
                                    :set-color (color :white)
                                    :ellipse 0 0 400 100
                                    :triangle 0 0 0 50 50 50)
                             :id :spider-bubble
                             :x 150
                             :y 400)
          spider-speech (assoc (label "" (color :black)
                                      :set-alignment (align :top)
                                      :set-wrap true)
                               :id :spider-speech
                               :x (+ (:x spider-bubble) 50)
                               :y (+ (:y spider-bubble) 0)
                               :width 300
                               :height 80)
          response (assoc (text-field "" (skin "uiskin.json")
                                      :set-alignment (align :right))
                          :id :response-field
                          :x (- (width screen) 220)
                          :y 330
                          :width 200)]
    [spider-bubble spider-speech response]))
  
(defscreen spider-screen
  :on-show
  (fn [screen _]
    (update! screen :camera (orthographic) :renderer (stage))
    (height! screen 600)
    (let [player (assoc (create-player)
                        :width 200 :height 200
                        :x (- (width screen) 200)
                        :y 100)]
      [player]))
  
  :on-render
  (fn [screen entities]
    (when (= :spider @current-screen-k)
      (clear! 0.2 0.2 0.2 1)
      (->> (for [entity entities]
             (case (:id entity)
               entity))
        (render! screen))))
  
  :on-enter
  (fn [screen entities]
    (let [spider (-> (:which-entity screen)
                   (assoc :width 200 :height 200
                          :x 50 :y 100))
          more-ents (make-speech-bubbles screen)]
      (->> (for [e more-ents]
             (case (:id e)
               :spider-speech (doto e (label! :set-text (:riddle spider)))
               :response-field (doto e (text-field! :set-text ""))
               e))
        (concat entities [spider]))))
  
  :on-key-down
  (fn [screen entities]
    (when (= :spider @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (when (= (:key screen) (key-code :enter))
          (let [field (find-by-id :response-field entities)
                wrote (-> (label! field :get-text)
                        (clojure.string/lower-case))
                spider (find-first :spider? entities)]
            (if (= wrote (:answer spider))
              ;; right
              (do (println "right!")
                (swap! player-data update-in [:riddles-done] conj (:id spider))
                (reset! current-screen-k :main))
              ;; wrong
              (println "wrong!"))
            entities
          )))))
  
  :on-ui-keyboard-focus-changed
  (fn [screen entities]
    (println (:event screen)) ; the FocusListener.FocusEvent - http://libgdx.badlogicgames.com/nightlies/docs/api/com/badlogic/gdx/scenes/scene2d/utils/FocusListener.FocusEvent.html
    (println (:actor screen)) ; the Actor - http://libgdx.badlogicgames.com/nightlies/docs/api/com/badlogic/gdx/scenes/scene2d/Actor.html
    (println (:focused? screen)) ; whether it is focused
    entities)
  
  :on-resize
  (fn [screen entities]
    (height! screen 600))
  
  ;:on-touch-down
  ;(fn [screen entities]
  ;  )
  )

(set-game-screen! main-screen spider-screen status-screen)
