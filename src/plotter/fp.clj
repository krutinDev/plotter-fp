(ns plotter.fp
  (:gen-class))

;; ---------------------------
;; Доменные типы и утилиты
;; ---------------------------

(def color->label
  {:black "чёрный"
   :red   "красный"
   :green "зелёный"})

(defn normalize-angle [a]
  (mod a 360.0))

(defn calc-new-position
  "Вычисляет новую позицию на расстояние distance при угле angle (в градусах),
   исходя из текущей позиции {:x .. :y ..}. Округляет координаты."
  [distance angle {:keys [x y]}]
  (let [rad (Math/toRadians angle)
        nx  (+ x (* distance (Math/cos rad)))
        ny  (+ y (* distance (Math/sin rad)))]
    {:x (long (Math/round nx))
     :y (long (Math/round ny))}))

(defn init-state
  "Инициализирует состояние плоттера."
  ([]
   (init-state {:x 0 :y 0} 0.0 :black :up))
  ([pos angle color carriage]
   {:pos pos
    :angle angle
    :color color
    :carriage carriage}))

;; ---------------------------
;; Чистые операции (без IO)
;; Каждая операция: state -> {:state <новое>, :events [<события>]}
;; События — обычные карты данных, которые потом можно отрендерить.
;; ---------------------------

(defn move-op
  "Возвращает операцию перемещения на distance.
   Если каретка опущена — генерируем событие рисования линии, иначе — перемещения."
  [distance]
  (fn [{:keys [pos angle color carriage] :as state}]
    (let [new-pos (calc-new-position distance angle pos)]
      {:state  (assoc state :pos new-pos)
       :events [(if (= carriage :down)
                  {:event :line :from pos :to new-pos :color color}
                  {:event :move :from pos :to new-pos :distance distance})]})))

(defn turn-op
  "Возвращает операцию поворота на delta градусов."
  [delta]
  (fn [{:keys [angle] :as state}]
    (let [new-angle (normalize-angle (+ angle delta))]
      {:state  (assoc state :angle new-angle)
       :events [{:event :turn :by delta :to new-angle}]})))

(def carriage-up-op
  (fn [state]
    {:state  (assoc state :carriage :up)
     :events [{:event :carriage :to :up}]}))

(def carriage-down-op
  (fn [state]
    {:state  (assoc state :carriage :down)
     :events [{:event :carriage :to :down}]}))

(defn set-color-op
  "Возвращает операцию установки цвета."
  [color]
  (fn [state]
    {:state  (assoc state :color color)
     :events [{:event :color :to color}]}))

(defn set-position-op
  "Возвращает операцию установки позиции (телепорт без линии)."
  [pos]
  (fn [state]
    {:state  (assoc state :pos pos)
     :events [{:event :position :to pos}]}))

;; ---------------------------
;; Комбинаторы и фигуры
;; ---------------------------

(defn times
  "Повторяет последовательность операций op-seq n раз, возвращает плоскую seq."
  [n op-seq]
  (apply concat (repeat n op-seq)))

(defn draw-triangle-ops
  "Последовательность операций для рисования треугольника со стороной size."
  [size]
  (concat
   [carriage-down-op]
   (times 3 [(move-op size) (turn-op 120.0)])
   [carriage-up-op]))

(defn draw-square-ops
  "Последовательность операций для рисования квадрата со стороной size."
  [size]
  (concat
   [carriage-down-op]
   (times 4 [(move-op size) (turn-op 90.0)])
   [carriage-up-op]))

;; ---------------------------
;; Исполнитель (reduce по операциям)
;; ---------------------------

(defn run
  "Запускает последовательность операций ops начиная с состояния init-state.
   Возвращает {:state <итоговое состояние> :events <список всех событий>}."
  [init-state ops]
  (reduce
   (fn [{:keys [state events]} op]
     (let [{st :state evs :events} (op state)]
       {:state st
        :events (into events evs)}))
   {:state init-state :events []}
   ops))

;; ---------------------------
;; Рендер событий в строки (IO отдельно)
;; ---------------------------

(defn format-event
  "Форматирует событие в текст, как в оригинальном задании."
  [{:keys [event] :as e}]
  (case event
    :line
    (let [{:keys [from to color]} e]
      (format "...Чертим линию из (%d, %d) в (%d, %d) используя %s цвет."
              (:x from) (:y from) (:x to) (:y to) (color->label color)))

    :move
    (let [{:keys [from distance]} e]
      (format "Передвигаем на %.1f от точки (%d, %d)"
              (double distance) (:x from) (:y from)))

    :turn
    (format "Поворачиваем на %.1f градусов" (double (:by e)))

    :carriage
    (if (= (:to e) :up) "Поднимаем каретку" "Опускаем каретку")

    :color
    (format "Устанавливаем %s цвет линии." (color->label (:to e)))

    :position
    (let [{:keys [to]} e]
      (format "Устанавливаем позицию каретки в (%d, %d)." (:x to) (:y to)))

    ;; fallback на сериализацию данных
    (pr-str e)))

(defn print-events!
  "Печатает события построчно."
  [events]
  (doseq [line (map format-event events)]
    (println line)))

;; ---------------------------
;; Пример «программы» как композиции операций
;; ---------------------------

(defn program
  "Программа из задания:
   1) Треугольник 100
   2) Перемещение позиции в (10, 10)
   3) Цвет красный
   4) Квадрат 80"
  []
  (concat
   (draw-triangle-ops 100.0)
   [(set-position-op {:x 10 :y 10})
    (set-color-op :red)]
   (draw-square-ops 80.0)))

;; ---------------------------
;; Точка входа (-main) для запуска через `clj -M -m plotter.fp`
;; ---------------------------

(defn -main [& _args]
  (let [init (init-state {:x 0 :y 0} 0.0 :black :up)
        {:keys [state events]} (run init (program))]
    (print-events! events)
    (println "\nИтоговое состояние:" state)))

;; ---------------------------
;; Удобно для REPL:
;; (comment
;;   (require '[plotter.fp :as p])
;;   (def init (p/init-state {:x 0 :y 0} 0.0 :black :up))
;;   (def res (p/run init (p/program)))
;;   (p/print-events! (:events res))
;;   (:state res)
;; )
;; ---------------------------
