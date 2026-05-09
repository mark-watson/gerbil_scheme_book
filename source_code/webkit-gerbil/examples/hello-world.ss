;;; hello-world.ss — Minimal webkit-gerbil example
;;;
;;; Displays a styled HTML page in a native macOS window.
;;; This is the simplest possible webkit-gerbil app.

(import "../webkit-gerbil")
(export main)

(def (main . args)
  (let ((a (create-app title: "Hello webkit-gerbil" width: 600 height: 400)))
    (load-html
     "<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
    background: linear-gradient(135deg, #0f0c29 0%, #302b63 50%, #24243e 100%);
    color: #e0e0e0;
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100vh;
    overflow: hidden;
  }
  .card {
    text-align: center;
    background: rgba(255,255,255,0.05);
    backdrop-filter: blur(20px);
    -webkit-backdrop-filter: blur(20px);
    border: 1px solid rgba(255,255,255,0.1);
    border-radius: 24px;
    padding: 48px 64px;
    box-shadow: 0 8px 32px rgba(0,0,0,0.3);
    animation: fadeIn 0.8s ease-out;
  }
  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(20px) scale(0.95); }
    to   { opacity: 1; transform: translateY(0) scale(1); }
  }
  h1 {
    font-size: 2.5em;
    font-weight: 700;
    background: linear-gradient(90deg, #a78bfa, #60a5fa, #34d399);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    margin-bottom: 12px;
  }
  p {
    font-size: 1.1em;
    color: rgba(255,255,255,0.6);
    line-height: 1.6;
  }
  .badge {
    display: inline-block;
    margin-top: 20px;
    padding: 6px 16px;
    font-size: 0.85em;
    background: rgba(167,139,250,0.15);
    border: 1px solid rgba(167,139,250,0.3);
    border-radius: 999px;
    color: #a78bfa;
  }
</style>
</head>
<body>
  <div class='card'>
    <h1>Hello, webkit-gerbil!</h1>
    <p>A native macOS window powered by Gerbil Scheme<br>
       and WebKit (WKWebView).</p>
    <span class='badge'>Gerbil + Gambit + Cocoa + WebKit</span>
  </div>
</body>
</html>"
     a)
    (app-run a)
    (app-destroy a)))
