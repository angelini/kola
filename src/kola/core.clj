(ns kola.core
  (:require [plumbing.core    :refer [fnk]]
            [plumbing.graph   :as    graph]
            [seesaw.core      :refer [frame canvas show! move! config!
                                      repaint! user-data select]]
            [seesaw.graphics  :refer [draw style string-shape]]
            [seesaw.color     :refer [color]]
            [seesaw.font      :refer [font]]
            [seesaw.event     :refer [listen]]
            [seesaw.keystroke :refer [keystroke]])
  (:import [javax.swing.KeyStroke]))

(def buffer-counter (atom 0))
(def window-counter (atom 0))
(def frame-counter (atom 0))

(defn create-buffer []
  {:id (swap! buffer-counter inc)
   :content []})

(defn create-window [size x y bid]
  {:id (swap! window-counter inc)
   :x x :y y
   :bx 0 :by 0
   :bid bid :mode "default"})

(defn create-frame [size bid]
  (let [w (create-window size 0 0 bid)]
    {:id   (swap! frame-counter inc)
     :ws   {(:id w) w}
     :size size}))

(defn create-cursor [fid wid]
  {:x 0 :y 0
   :fid fid :wid wid})

(defn create-state [size]
  (let [b (create-buffer)
        f (create-frame size (:id b))
        w (-> f :ws first val)]
    {:bs   {(:id b) b}
     :fs   {(:id f) f}
     :cur  (create-cursor (:id f) (:id w))}))

(def links*
  {:f    (fnk [fs cur] (get fs (:fid cur)))
   :ws   (fnk [f]      (:ws f))
   :w    (fnk [ws cur] (get ws (:wid cur)))
   :b    (fnk [bs w]   (get bs (:bid w)))
   :mode (fnk [w]      (:mode w))})

(def links (graph/compile links*))

(defn assoc-f [state f]
  (if (nil? f)
    state
    (assoc-in state [:fs (:id f)] f)))

(defn assoc-ws [state ws]
  (if (nil? ws)
    state
    (let [fid (get-in state [:cur :fid])]
      (assoc-in state [:fs fid :ws] ws))))

(defn assoc-w [state w]
  (if (nil? w)
    state
    (let [{:keys [fid wid]} (:cur state)]
      (assoc-in state [:fs fid :ws wid] w))))

(defn assoc-b [state b]
  (if (nil? b)
    state
    (assoc-in state [:fs (:id b)] b)))

(defn assoc-mode [state mode]
  (if (nil? mode)
    state
    (let [{:keys [fid wid]} (:cur state)]
      (assoc-in state [:fs fid :ws wid :mode] mode))))

(defn update-state [state update]
  (let [linked     (into state (links state))
        new-linked (update linked)]
    (-> state
        (merge (select-keys new-linked [:bs :fs :cur]))
        (assoc-f    (:f new-linked))
        (assoc-ws   (:ws new-linked))
        (assoc-w    (:w new-linked))
        (assoc-b    (:b new-linked))
        (assoc-mode (:mode new-linked)))))

(defn move-cursor [x y]
  (fnk [cur]
    {:cur (-> cur
              (update-in [:x] #(+ % x))
              (update-in [:y] #(+ % y)))}))

(defn select-fn [e]
  (condp = (KeyStroke/getKeyStrokeForEvent e)
    (keystroke "RIGHT") (move-cursor 1 0)
    (keystroke "LEFT")  (move-cursor -1 0)
    (keystroke "UP")    (move-cursor 0 -1)
    (keystroke "DOWN")  (move-cursor 0 1)
    identity))

(def text-style (style :foreground (color 0 0 0)
                       :font       (font :name :monospaced
                                         :size 10)))

(defn paint [c g]
  (let [state (user-data c)
        cur   (:cur state)]
    (draw g (string-shape 0 10 (str "Cursor: " (:x cur) "," (:y cur))) text-style)))

(defn key-pressed [e]
  (let [canvas    (select e [:#canvas])
        state     (user-data canvas)
        update-fn (select-fn e)]
    (config! canvas :user-data (update-state state update-fn))
    (repaint! e)))

(defn start []
  (-> (frame
        :title     "Kola"
        :width     500
        :height    500
        :on-close  :dispose
        :content   (canvas :id         :canvas
                           :paint      #(#'paint %1 %2)
                           :user-data  (create-state [82 48])
                           :background "#BBBBDD"))
      (doto (.setAlwaysOnTop true))
      show!
      (move! :to [1420, 700])
      (listen :key-pressed #'key-pressed)))

