# 練習用 Emacs-Lisp

我有臉書粉絲專頁的一批影片清單，並且由於熟悉 emacs 操作的關係，想要借用這個程式語言（沒錯， emacs 是程式語言）來簡便完成工作。

不過，其實練習過程不是太簡便啦。

### 練習過程相關紀錄

1. 由 emacs 發出 Http 請求 https://yauhsien.wordpress.com/2018/01/28/%e9%80%81%e5%87%ba-http-%e8%ab%8b%e6%b1%82/
1. 順著抓到的 JSON 寫作主程式 https://yauhsien.wordpress.com/2018/01/28/3584/
1. 發現 emacs 24.5 無法處理 ISO 8601 時間文字標記 https://yauhsien.wordpress.com/2018/01/28/emacs-lisp-parse-iso8601-time-string/

### 為什麼我說 emacs 是程式語言

因為當我想要使用 `(json-read)` 這個程式時，理解到它將 emacs buffer 當做運作環境及運作對象，從游標所在的位置開始，讀取之後寫在 buffer 上的 JSON 文字，直到讀完一個單元（如果游標停在一個字串開始，它就讀完那一句字串；如果游標停在一個 JSON object 開始，它就讀完那個 object ）。由此看，程式的運作與編輯器密切相關。

而 Paul Graham 的 "ANSI Common Lisp" 甚至說，當人們使用 emacs ，敲打每一個字的時候，其實是使用了 Emacs-Lisp 程式接收按鍵信號，並且將按鍵結果反映在 emacs buffer 上。
