(load-game-file "entities.clj")

(declare main-screen status-screen spider-screen)

;; one of :main, :spider
(def current-screen-k (atom :main))

(defn narrate
  [text]
  (screen! status-screen :on-narration :say text))

(def start-story "You come out of the candy house into the bright daylight. Right! Come on, be brave!")

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
              (do (screen! spider-screen :on-enter
                           :which-entities {:player player :spider e})
                (reset! current-screen-k :spider)
                e))
            ;; meet sharks
            (:shark? e)
            (do
              
              e)
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
                        :walk-layers player-layers
                        :x 5 :y (- (dec map-height) 24))
          spiders [(assoc (create-spider screen "spider-1")
                          :walk-layers #{"pits"}
                          :in-pits? true
                          :riddle (str "First tell me where you battle and fight, "
                                       "then what is in the middle of battle, "
                                       "and what sounds like the search for a hard to find word. "
                                       "Put them together to make something hard to pick up.")
                          :answer "water")
                   (assoc (create-spider screen "spider-2")
                          :riddle (str "Put riddle here.")
                          :answer "foo")]
          pool-shark (create-shark screen "pool-shark")
          rope (assoc (create-entity-from-object-layer screen "rope")
                      :item? true
                      :in-pits? true)
          sword (assoc (create-entity-from-object-layer screen "sword")
                       :item? true)
          floaty (assoc (create-entity-from-object-layer screen "floaty")
                       :item? true)
          wand (assoc (create-entity-from-object-layer screen "wand")
                       :item? true)
          lava-step (assoc (create-entity-from-object-layer screen "lava-step")
                       :item? true)
          volcs [(create-eruption screen "volcano-1")
                 (create-eruption screen "volcano-2")]
          ]
      (add-timer! screen :eruption 5 5)
      (concat [player rope sword floaty wand lava-step]
              [pool-shark]
              spiders
              volcs)))
  
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
                 (move screen)
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
        :sword (if (and (not (:in-pits? player))
                        (on-layer-ok? screen player "bridges"))
                 (do (narrate "You slash the rope bridge and tumble into the pit below.")
                   (->> (assoc player
                               :walk-layers #{"pits"}
                               :in-pits? true)
                     (conj (remove :player? entities))))
                   (do (narrate "You slash something unnecessarily and look around foolishly.")
                     nil))
        :rope (if (and (:in-pits? player)
                       (on-layer-ok? screen player "bridges"))
                (do (narrate "Ah, this rope is so useful!!!")
                  (->> (assoc player
                              :walk-layers player-layers
                              :in-pits? nil)
                    (conj (remove :player? entities))))
                (do (narrate "Oops, not there!!!")
                  nil))
        :floaty (if (and (some #(touching-layer? screen player %) float-layers)
                         (not (:floating? player)))
                  (do (narrate "keep up the floating!!!")
                    (->> (assoc player
                               :walk-layers (into (:walk-layers player) float-layers)
                               :floating? true)
                     (conj (remove :player? entities))))
                  (do (narrate "Don't have time to play with toys now!!!")
                    nil
                    ))
        :wand (do
                (narrate "")
                nil)
        :lava-step (if (touching-layer? screen player "lava")
                     (do (narrate "Hop, skip and jump!!!")
                       )
                     (do (narrate "Be careful! Don't drop it on your foot!!!")
                       ))
        )))
  
  :on-solve-riddle
  (fn [screen entities]
    (narrate "Your first triumph has come!")
    (swap! player-data update-in [:riddles-done] conj (:spider-id screen))
    ;; TODO key
    entities
    )
  
  :on-give-up-riddle
  (fn [screen entities]
    (narrate "This is bad...")
    (swap! player-data update-in [:riddles-done] conj (:spider-id screen))
    entities
    )
                  
  :on-key-down
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (->>
          (cond->
            player
            ;; jump
            (= (:key screen) (key-code :space))
            (start-jump)
            )
          (conj (remove :player? entities))))))
  
  :on-touch-down
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (let [x (game :x)
              y (game :y)
              width (game :width)
              height (game :height)]
          (println x y width height)
          (->>
            (cond->
              player
              ;; jump
              (and (< (* width 0.45) x (* width 0.55))
                   (< (* height 0.45) y (* width 0.55)))
              (start-jump)
              )
            (conj (remove :player? entities)))))))
  )

;;;; status screen - items, health

(def button->item-id (atom {}))

(defscreen status-screen
  :on-show
  (fn [screen _]
    (let [screen (update! screen :camera (orthographic) :renderer (stage))]
      (height! screen 500)
      [(assoc (vertical [] :left :reverse)
              :id :item-table
              :y 0)
       (assoc (label "" (color :yellow))
              :id :health
              :x (- (width screen) 100) :y 10)
       (assoc (label start-story (color :white))
              :id :narration
              :x (width screen)
              :y (- (height screen) 100)
              :height 50)]
    ))
  
  ;; when calling this, give :which-entity with the entity
  :on-pick-up-item
  (fn [screen entities]
    (let [e (-> (:which-entity screen)
              (update-in [:width] * pixels-per-tile)
              (update-in [:height] * pixels-per-tile))
          ui-skin (skin "uiskin.json")
          button (text-button " Use " ui-skin)
          item-table (find-by-id :item-table entities)
          held-e (horizontal [(image e) button])
          ]
      (case (:id e)
        :sword (narrate "Your holely sword has been brought to you!")
        :rope (narrate "You find a small rope in a pit.")
        :floaty (narrate "What a surprise! A floaty!")
        :wand (narrate "A wand! Do some magic!")
        :lava-step (narrate "You find a big stepping stone!"))
      (swap! button->item-id assoc (:object button) (:id e))
      (add! item-table held-e)
      entities))
  
  :on-ui-changed
  (fn [screen entities]
    (let [item-id (get @button->item-id (:actor screen))]
      (screen! main-screen :on-use-item :item-id item-id)
      entities))
  
  :on-key-down
  (fn [screen entities]
    (let [item-ids (vals @button->item-id)]
      (cond
        (= (:key screen) (key-code :num-1))
        (do (screen! main-screen :on-use-item :item-id
                     (nth item-ids 0))
          entities)
        )))
  
  :on-narration
  (fn [screen entities]
    (for [e entities]
      (if (= :narration (:id e))
        (do
          (label! e :set-text (:say screen))
          (assoc e :x (width screen) :counter 0))
        e)))
  
  :on-render
  (fn [screen entities]
    (->> (for [e entities]
           (case (:id e)
             :health (doto e
                       (label! :set-text (str "health " (:health @player-data))))
             :narration (cond
                          (nil? (:counter e))
                          e
                          (> (:counter e 0) (* 60 5))
                          (do (label! e :set-text "")
                            (dissoc e :counter))
                          :else
                          (-> e
                            (update-in [:x] #(int (* % 0.9)))
                            (update-in [:counter] inc)))
             e))
         (render! screen)))
  
  :on-resize
  (fn [screen entities]
    (height! screen 500)))


;;;; spider screen - sub-game for riddles, fighting

(defn make-speech-bubbles
  [screen]
  (let [spider-bubble (assoc (shape :filled
                                    :set-color (color :white)
                                    :ellipse 0 0 400 160
                                    :triangle -10 -10 0 80 60 50)
                             :id :spider-bubble
                             :x 150
                             :y 300)
        spider-speech (assoc (label "" (color :black)
                                    :set-alignment (align :top)
                                    :set-wrap true)
                             :id :spider-speech
                             :x (+ (:x spider-bubble) 50)
                             :y (+ (:y spider-bubble) 10)
                             :width 300
                             :height 120)
        ui-skin (skin "uiskin.json")
        response (assoc (text-field "" ui-skin
                                    :set-alignment (align :left))
                        :id :response-field
                        :x (- (width screen) 220)
                        :y 400
                        :width 200)
        response-but (assoc (text-button " Reply " ui-skin)
                            :id :response-button
                            :x (+ (:x response) 50)
                            :y (- (:y response) 30))
        give-up-but (assoc (text-button "give up" ui-skin)
                            :id :give-up-button
                            :x (+ (:x response) 120)
                            :y (- (:y response) 30))]
    [spider-bubble spider-speech response response-but give-up-but]))
  
(defn spider-say!
  [entities text]
  (label! (find-by-id :spider-speech entities)
          :set-text text))

(defscreen spider-screen
  :on-show
  (fn [screen _]
    (let [screen (update! screen :camera (orthographic) :renderer (stage))]
      (height! screen 600)
      (make-speech-bubbles screen)))
  
  :on-render
  (fn [screen entities]
    (when (= :spider @current-screen-k)
      (clear! 0.2 0.2 0.2 1)
      (->> entities
        (render! screen))))
  
  :on-enter
  (fn [screen entities]
    (let [in-ents (:which-entities screen)
          spider (assoc (:spider in-ents)
                        :width 200 :height 200
                        :x 100 :y 100)
          player (assoc (:player in-ents)
                        :width 200 :height 200
                        :x (- (width screen) 200)
                        :y 100)]
      (spider-say! entities (:riddle spider))
      (into entities
            [spider player])))
  
  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :say-riddle (let [spider (find-first :spider? entities)]
                    (spider-say! entities (:riddle spider))
                    entities)
      :solved-riddle (let [spider (find-first :spider? entities)]
                       (screen! main-screen :on-solve-riddle :spider-id (:id spider))
                       (reset! current-screen-k :main)
                       ;; remove actors that came in
                       (remove #(or (:player? %) (:spider? %)) entities))
      :give-up (let [spider (find-first :spider? entities)]
                 (screen! main-screen :on-give-up-riddle :spider-id (:id spider))
                 (reset! current-screen-k :main)
                 ;; remove actors that came in
                 (remove #(or (:player? %) (:spider? %)) entities))
      ))
  
  :on-ui-changed
  (fn [screen entities]
    (stage! screen :set-keyboard-focus nil)
    (when-let [player (find-by-id :player entities)]
      (if (= (:actor screen) (:object (find-by-id :response-button entities)))
          (let [field (find-by-id :response-field entities)
                wrote (-> (text-field! field :get-text)
                        (clojure.string/lower-case))
                spider (find-first :spider? entities)]
            (if (= wrote (:answer spider))
              (do
                (spider-say! entities "Correct.")
                (add-timer! screen :solved-riddle 2)
                entities)
              (do
                (spider-say! entities "Wrong.")
                (add-timer! screen :say-riddle 2)
                entities)
              ))
          ;; gave up
          (do
            (spider-say! entities "qqqqqqqqqqqqq")
            (add-timer! screen :give-up 2)
            entities)
          )))
  
  :on-resize
  (fn [screen entities]
    (height! screen 600))
  )

(set-game-screen! main-screen spider-screen status-screen)
