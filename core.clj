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

(defn burn
  [player attack]
  (if (:on-fire? player)
    player
    (do
      (narrate "Hi.")
      (particle-effect! attack :start)
      (assoc player :on-fire? true))))

(defn shark-bite
  [player attack]
  (if (:bleeding? player)
    player
    (do
      (narrate "HELP!!!!!!")
      (particle-effect! attack :start)
      (assoc player :bleeding? true))))

(defn poisoning
  [player attack]
  player
  )

(defn interact
  [entities]
  (when-let [player (find-by-id :player entities)]
    (->>
     entities
     (reduce (fn [entities e]
               ;; something we can touch?
               (if (or (:player? e)
                       (not (near-entity? player e 1))
                       (not= (:in-pits? e) (:in-pits? player)))
                 entities
                 ;; can touch
                 (cond
                  ;; pick up items
                  (:item? e)
                  (do (screen! status-screen :on-pick-up-item :which-entity e)
                      ;; remove from screen
                      (remove #(= (:id %) (:id e)) entities))
                  ;; meet spiders
                  (:spider? e)
                  (cond
                   ;; attack
                   (:chasing? e)
                   entities
                   ;; already done this spider's riddle, ignore it
                   (get (:riddles-done player) (:id e))
                   entities
                   ;; new riddle
                   :else
                   (do (screen! spider-screen :on-enter
                                :which-entities {:player player :spider e})
                       (reset! current-screen-k :spider)
                       entities))
                  ;; meet sharks
                  (:shark? e)
                  (update-by-id entities :player shark-bite (find-by-id :blood entities))
                  ;; meet poison
                  (= :poison (:id e))
                  (update-by-id entities :player poisoning (find-by-id :poison entities))
                  ;; meet volcanos
                  (:volcano? e)
                  (if (particle-effect! e :is-complete)
                    ;; not currently erupting, pass through
                    entities
                    ;; hit by eruption
                    (update-by-id entities :player burn (find-by-id :burn entities)))
                  ;; all others
                  :else
                  entities)))
             entities))))

(defn act
  [screen entities entity]
  (cond
   (:player? entity)
   (cond-> (merge entity (get-player-velocities entity))
           ;; if not touching water any more
           (and (:floating? entity)
                (not (some #(touching-layer? screen entity %) float-layers)))
           (assoc :floating? false
                  :walk-layers (apply disj (:walk-layers entity) float-layers))
           ;; if not on fire any more
           (and (:on-fire? entity)
                (particle-effect! (find-by-id :burn entities) :is-complete))
           (assoc :on-fire? false)
           ;; if not bleeding any more
           (and (:bleeding? entity)
                (particle-effect! (find-by-id :blood entities) :is-complete))
           (assoc :bleeding? false)
           ;; if touching lava
           (and (not (:jumping? entity))
                (on-layer-ok? screen entity "lava"))
           (burn (find-by-id :burn entities))
           )
   ;; spiders
   (:spider? entity)
   (cond
    (and (:chasing? entity) (not (:spitting? entity)))
    (do (add-timer! screen :poison-spit 0.1)
      (assoc entity :spitting? true))
    (:chasing entity)
    (chase entity (find-by-id :player entities))
    :else
    entity)
   ;; pool shark
   (= :pool-shark (:id entity))
   (cond
    (let [player (find-by-id :player entities)]
      (and (touching-layer? screen player "Shark pool")
           (:floating? player)
           (not (:invisible? player))))
    (chase entity (find-by-id :player entities))
    ;; go round in circles
    :else
    (go-round-and-round entity))
   ;; lake sharks
   (:shark? entity)
   (cond
    true
    (chase entity (find-by-id :player entities)))
   ;; volcanos
   (:volcano? entity)
   (doto entity
     (particle-effect! :update (graphics! :get-delta-time)))
   ;; poison
   (= :poison (:id entity))
   entity
   ;; sticky attacks
   (:attack? entity)
   (let [player (find-by-id :player entities)]
     (particle-effect! entity :update (graphics! :get-delta-time))
     (merge entity (select-keys player [:x :y :width :height])))
   :else
   entity))

(defn render-with-overlaps
  [screen entities]
  (render-map-fixed! screen :without "bridges")
  (draw! screen (filter :in-pits? entities))
  (render-map-fixed! screen :with "bridges")
  (draw! screen (remove :in-pits? entities))
  entities)

(defn try-jump
  [player]
  (if (:in-pits? player)
    (narrate "Bbbbaaaadddd!!!"))
  (start-jump player))

(defscreen main-screen
  :on-show
  (fn [screen _]
    ;(reset! current-screen-k :main)
    (let [renderer (orthogonal-tiled-map "level.tmx" (/ 1 pixels-per-tile))
          screen (update! screen :camera (orthographic) :renderer renderer)
          player (assoc (create-player)
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
          pool-shark (assoc (create-shark screen "pool-shark")
                       :focus-point [(:x lava-step) (:y lava-step)])
          volcs [(create-volcano screen "volcano-1" "fire.p")
                 (create-volcano screen "volcano-2" "fire2.p")
                 ]
          attacks [(assoc (particle-effect "burn.p" :scale-effect 0.02)
                     :id :burn
                     :attack? true
                     :x 0 :y 0)
                   (assoc (particle-effect "blood.p" :scale-effect 0.01)
                     :id :blood
                     :attack? true
                     :x 0 :y 0)
                   (assoc (particle-effect "poison.p" :scale-effect 0.02)
                     :id :poison
                     :attack? true
                     :x 0 :y 0)
                   ]
          ]
      (add-timer! screen :eruption-1 5 5)
      (add-timer! screen :eruption-2 6 5)
      (concat [player rope sword floaty wand lava-step pool-shark]
              spiders
              volcs
              attacks)))
  
  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :eruption-1 (update-by-id entities :volcano-1
                                #(doto % (particle-effect! :start)))
      :eruption-2 (update-by-id entities :volcano-2
                                #(doto % (particle-effect! :start)))
      :poison-spit
      (when-let [spider (find-first #(and (:spider? %) (:spitting? %)) entities)]
        (let [player (find-by-id :player entities)]
          (add-timer! screen :poison-spit-done 4)
          (update-by-id entities :poison (fn [e]
                                           (particle-effect! e :start)
                                           (assoc e :x (:x spider)
                                                  :y (:y spider)
                                                  :x-velocity (* 5.0 (- (:x player) (:x spider)))
                                                  :y-velocity (* 5.0 (- (:y player) (:y spider))))))))
      :poison-spit-done
      (for [e entities]
        (if (:spider? e)
          (assoc e :spitting? false)
          e))
      ))
  
  :on-render
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (clear!)
      (->> entities
        (map (fn [entity]
               (->> entity
                    (act screen entities)
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
                     (update-by-id entities :player
                                   assoc :walk-layers #{"pits"}
                                   :in-pits? true))
                   (do (narrate "You slash something unnecessarily and look around foolishly.")
                     nil))
        :rope (if (and (:in-pits? player)
                       (on-layer-ok? screen player "bridges"))
                (do (narrate "Ah, this rope is so useful!!!")
                    (update-by-id entities :player
                                  assoc :walk-layers player-layers
                                  :in-pits? nil))
                (do (narrate "Oops, not there!!!")
                  nil))
        :floaty (if (and (some #(touching-layer? screen player %) float-layers)
                         (not (:floating? player)))
                  (do (narrate "keep up the floating!!!")
                      (update-by-id entities :player
                                    assoc :walk-layers (into (:walk-layers player) float-layers)
                                    :floating? true))
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
    (when-let [player (find-by-id :player entities)]
      (case (count (:riddles-done player))
        0 (narrate "Your first triumph has come!")
        1 (narrate "Two's the one!")
        2 (narrate "tut tut! 3333!")
        3 (narrate "You've done it!!!")
        )
      (update-by-id entities :player
                    (fn [e]
                      (-> e
                          (update-in [:riddles-done] conj (:spider-id screen))
                          (update-in [:keys-won] conj (:spider-id screen)))))
      )
    ;; TODO key
    )
  
  :on-give-up-riddle
  (fn [screen entities]
    (when-let [player (find-by-id :player entities)]
      (narrate "This is bad...Ahhhhhhhh!!!!!!!")
      (-> entities
          (update-by-id :player
                        update-in [:riddles-done] conj (:spider-id screen))
          (update-by-id (:spider-id screen)
                        assoc :chasing? true))))

  :on-key-down
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (if  (= (:key screen) (key-code :space))
          (update-by-id entities :player try-jump)
          ))))
  
  :on-touch-down
  (fn [screen entities]
    (when (= :main @current-screen-k)
      (when-let [player (find-by-id :player entities)]
        (let [x (game :x)
              y (game :y)
              width (game :width)
              height (game :height)]
          (if (and (< (* width 0.45) x (* width 0.55))
                   (< (* height 0.45) y (* width 0.55)))
            (update-by-id entities :player try-jump)
            )))))
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
       (assoc (label "health 100" (color :yellow))
              :id :health-text
              :x (- (width screen) 100) :y 10)
       (assoc (shape :filled
                     :set-color (color :black)
                     :rect 0 0 10 102
                     :set-color (color :gray)
                     :rect 1 1 8 100)
         :id :health-bar-bg
         :x (- (width screen) 20)
         :y 10)
       (assoc (shape :filled
                     :set-color (color :green)
                     :rect 1 1 8 100)
         :id :health-bar
         :x (- (width screen) 20)
         :y 10
         :scale-y 1.0)
       (assoc (label start-story (color :white))
              :id :narration
              :x 10
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
    (update-by-id entities :narration
                  (fn [e]
                    (label! e :set-text (:say screen))
                    (assoc e :x (width screen) :counter 0))))
  
  :on-render
  (fn [screen entities]
    (->> (for [e entities]
           (case (:id e)
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
    (height! screen 500)
    (for [e entities]
      (case (:id e)
        :health-bar-bg (assoc e :x (- (width screen) 20))
        :health-bar (assoc e :x (- (width screen) 20))
        :health-text (assoc e :x (- (width screen) 100))
        e)))
  )


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
            (spider-say! entities "You... I want you...!")
            (add-timer! screen :give-up 2)
            entities)
          )))
  
  :on-resize
  (fn [screen entities]
    (height! screen 600))
  )

(set-game-screen! main-screen spider-screen status-screen)
