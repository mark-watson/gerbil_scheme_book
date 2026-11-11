;; test-cache.ss
(import :std/test
        "daily_use/cache")

(export main)

(def cache-suite
  (test-suite "daily_use cache tests"
    (test-case "keyword-ranked lookup"
      (let* ((tmp-path "/tmp/test-daily-cache.sexp")
             (_ (when (file-exists? tmp-path) (delete-file tmp-path)))
             (c (make-cache tmp-path)))
        (cache-add c "The weather in San Francisco is foggy and cool")
        (cache-add c "San Francisco restaurants and food guide")
        (cache-add c "Tokyo weather and restaurants in Japan")
        ;; query with two keywords: San, Francisco
        (let ((res (cache-lookup c '("san" "francisco"))))
          (check (length res) => 2)
          (check (pair? (member "The weather in San Francisco is foggy and cool" res)) => #t)
          (check (pair? (member "San Francisco restaurants and food guide" res)) => #t))
        ;; query with three keywords: San, Francisco, weather
        (let ((res (cache-lookup c '("san" "francisco" "weather"))))
          ;; "The weather in San Francisco is foggy and cool" matches 3 keywords;
          ;; "San Francisco restaurants and food guide" matches 2;
          ;; "Tokyo weather..." matches 1.
          (check (car res) => "The weather in San Francisco is foggy and cool")
          (check (cadr res) => "San Francisco restaurants and food guide")
          (check (caddr res) => "Tokyo weather and restaurants in Japan"))
        (cache-close c)
        (delete-file tmp-path)))))

(def (main . args)
  (run-tests! cache-suite)
  (test-report-summary!)
  (exit (case (test-result) ((OK) 0) (else 1))))
