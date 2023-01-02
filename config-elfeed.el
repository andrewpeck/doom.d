;;; config-elfeed.el -*- lexical-binding: t; -*-

;; Run `elfeed-update' every 8 hours
(run-at-time nil (* 8 60 60) #'elfeed-update)

(after! elfeed
  (setq elfeed-feeds
        '("https://www.evalapply.org/index.xml"
          ;; "https://hackaday.com/blog/feed/"
          "https://nullprogram.com/feed/"
          "https://bzg.fr/index.xml"
          "https://www.mattblaze.org/blog/rss20.xml"
          "https://jackrusher.com/feed.xml"
          "http://mbork.pl/?action=rss;days=30;all=0;showedit=0"
          "https://isc.sans.edu/rssfeed_full.xml"
          "https://watchguy.co.uk/feed/"
          "https://sachachua.com/blog/feed/")))
