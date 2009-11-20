(ns plugin-test
  (:use plugin-loader))

(println (pr-str (dispatch plugins ":jjames!n=user@li112-65.members.linode.com PRIVMSG #mindhed :@count")))
(println (pr-str (dispatch plugins  ":jjames!n=user@li112-65.members.linode.com PRIVMSG #mindhed :a little noisy")))
