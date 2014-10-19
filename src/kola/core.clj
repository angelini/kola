(ns kola.core
  (:require [plumbing.core    :refer [fnk defnk]]
            [plumbing.graph   :as    graph]
            [seesaw.core      :refer [frame canvas show! move! config!
                                      repaint! user-data select]]
            [seesaw.graphics  :refer [draw style string-shape]]
            [seesaw.color     :refer [color]]
            [seesaw.font      :refer [font]]
            [seesaw.event     :refer [listen]]
            [seesaw.keystroke :refer [keystroke]])
  (:import [javax.swing.KeyStroke]))

(defn update-values [m f & args]
  (reduce
    (fn [r [k v]] (assoc r k (apply f v args)))
    {} m))

(def buffer-counter (atom 0))
(def window-counter (atom 0))
(def frame-counter (atom 0))

(defn create-buffer []
  {:id (swap! buffer-counter inc)
   :content []})

(defn create-window [size x y bid]
  {:id   (swap! window-counter inc)
   :bid  bid
   :x x  :y y
   :bx 0 :by 0
   :size size
   :mode "default"})

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

(defn split-horiz-size [size]
  (let [[width height] size
        half (/ height 2)]
    (if (even? height)
      [[width half]       [width (- half 1)]]
      [[width (int half)] [width (int half)]])))

(defn split-vert-size [size]
  (let [[width height] size
        half (/ width 2)]
    (if (even? width)
      [[half height]       [(- half 1) height]]
      [[(int half) height] [(int half) height]])))

(defnk split-horiz [bs ws w]
  (let [[top-size bottom-size] (split-horiz-size (:size w))
        bottom-x (:x w)
        bottom-y (+ (:y w) (last top-size) 1)
        nb (create-buffer)
        nw (create-window bottom-size
                          bottom-x bottom-y
                          (:id nb))]
    {:bs  (assoc bs (:id nb) nb)
     :ws  (assoc ws (:id nw) nw)
     :w   (assoc w :size top-size)}))

(defnk split-vert [bs ws w]
  (let [[left-size right-size] (split-vert-size (:size w))
        right-x (+ (:x w) (first left-size) 1)
        right-y (:y w)
        nb (create-buffer)
        nw (create-window right-size
                          right-x right-y
                          (:id nb))]
    {:bs  (assoc bs (:id nb) nb)
     :ws  (assoc ws (:id nw) nw)
     :w   (assoc w :size left-size)}))

(defn select-fn [e]
  (condp #(= (keystroke %1) %2) (KeyStroke/getKeyStrokeForEvent e)
    "RIGHT" (move-cursor 1 0)
    "LEFT"  (move-cursor -1 0)
    "UP"    (move-cursor 0 -1)
    "DOWN"  (move-cursor 0 1)
    "H"     split-horiz
    "V"     split-vert
    identity))

(defn pad-str [s len]
  (format (str "%-" len "s") s))

(defn gen-str [c len]
  (apply str (repeat len c)))

(defn line-at [b x y len]
  (let [content (:content b)
        line (get content y)]
    (if (or (nil? line)
            (>= x (count line)))
      (gen-str " " len)
      (pad-str (subs line x) len))))

(defn visible [w bs]
  (let [b (get bs (:bid w))
        {:keys [size x y bx by]} w]
    (vec (for [y (range (last size))]
      (line-at b bx (+ y by) (first size))))))

(defn has-line-at? [w y]
  (let [start (:y w)
        height (get-in w [:size 1])]
    (and (>= y start)
         (< y (+ start height)))))

(defn content-line [ws-content wid w y]
  (let [content (get ws-content wid)]
    (get content (- y (:y w)))))

(defn visible-strs [ws ws-content y]
  (->> ws
       (filter (fn [[wid w]] (has-line-at? w y)))
       (map (fn [[wid w]]
              {:x    (:x w)
               :line (content-line ws-content wid w y)}))))

(defn pad-left [width c s]
  (let [len (- width (count s))
        fill (gen-str c len)]
    (str fill s)))

(defn combine-strs [strs width]
  (let [sorted (sort-by #(* -1 (:x %)) strs)]
    (->> sorted
      (reduce
        (fn [acc s]
          (let [{:keys [x line]} s
                fill (gen-str "x" (- (- width (count acc))
                                     (+ x (count line))))]
            (str line fill acc)))
        "")
      (pad-left width "x"))))

(defnk generate-display [f bs ws w]
  (let [[width height] (:size f)
        ws-content (update-values ws visible bs)]
    (for [y (range height)
          :let [strs (visible-strs ws ws-content y)
                combined (combine-strs strs width)]]
        (combine-strs strs width))))

(def text-style (style :foreground (color 0 0 0)
                       :font       (font :name :monospaced
                                         :size 10)))

(defn paint-line [g y line]
  (draw g (string-shape 0 (* y 10) line) text-style))

(defn paint [c g]
  (let [state   (user-data c)
        cur     (:cur state)
        display (generate-display (into state (links state)))]
    (doall (map-indexed #(paint-line g %1 %2) display))))

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

