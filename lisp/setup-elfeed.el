;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Elfeed
;;------------------------------------------------------------------------------

(use-package! elfeed

  :config

  ;; Run `elfeed-update' every 8 hours
  (run-at-time nil (* 8 60 60) #'elfeed-update)

  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "U") #'elfeed-update)

  (setq elfeed-db-directory (concat doom-user-dir "elfeed"))

  (setq elfeed-feeds
        '("https://www.evalapply.org/index.xml"
          ;; "https://hackaday.com/blog/feed/"
          "http://yummymelon.com/devnull/feeds/all.atom.xml"
          "https://simonwillison.net/atom/everything/"
          "https://krebsonsecurity.com/feed/"
          "https://ciechanow.ski/atom.xml"
          "https://danluu.com/atom.xml"
          "https://www.righto.com/feeds/posts/default"
          "https://jvns.ca/atom.xml"
          "https://devblogs.microsoft.com/oldnewthing/feed/"
          "https://www.jeffgeerling.com/blog.xml"
          "https://daniel.haxx.se/blog/feed/"
          ;; "https://planet.emacslife.com/atom.xml"
          "https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-only.xml"
          "https://nullprogram.com/feed/"
          "https://tracker.orgmode.org/bugs.rss"
          "https://bzg.fr/index.xml"
          "https://www.mattblaze.org/blog/rss20.xml"
          "https://jackrusher.com/feed.xml"
          "https://endlessparentheses.com/atom.xml"
          "http://mbork.pl/?action=rss;days=30;all=0;showedit=0"
          "https://watchguy.co.uk/feed/"
          "https://sachachua.com/blog/feed/")))
