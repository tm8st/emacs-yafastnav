;;; yafastnav.el --- yet another fastnav.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <http://twitter.com/tm8st>
;; Version: 0.21
;; Keywords: convenience, move, fastnav

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a  copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 表示されているウィンドウ内への移動の高速化のための拡張です。
;; 最初に正規表現で画面内の候補をリストアップし、ショートカットキーと対応づけ、
;; 次にショートカットキーを入力することで、目的の位置へ移動します。

;; 設定例

;; (require 'yafastnav)
;; (global-set-key (kbd "C-l C-h") 'yafastnav-jump-to-current-screen)
;; (global-set-key (kbd "C-l C-.") 'yafastnav-jump-to-forward)
;; (global-set-key (kbd "C-l C-r") 'yafastnav-jump-to-backward)

;; TODO: yafastnav-jump-to-backwardがうしろ向き検索になっていないのに対処

;;; Code:

;;;-------------------------------
;;; variables
;;;-------------------------------
(defgroup yafastnav nil "yet another fastnav."
  :prefix "yafastnav-" :group 'convenience)

(defcustom yafastnav-default-regex
  "\\([a-zA-Z_?]+[a-zA-Z0-9_-]+\\)"
  "リストアップする要素の指定用正規表現"
  :type 'regexp
  :group 'yafastnav)

(defcustom yafastnav-text-mode-regex
  "\\([a-zA-Z_?あ-んア-ン]+[a-zA-Z0-9_-あ-んア-ン]+\\)"
  "テキストモード用リストアップする要素の指定用正規表現"
  :type 'regexp
  :group 'yafastnav)

(defcustom yafastnav-mode-regex-alist
  '(
    (fundamental-mode . yafastnav-text-mode-regex)
    (text-mode . yafastnav-text-mode-regex)
    )
  "モード別の正規表現の指定用連想リスト"
  :type '(repeat (symbol . regexp))
  :group 'yafastnav)

(defcustom yafastnav-more-shortcutkey ? 
  "次の要素のリストアップの実行ショートカットキー
  yafastnav-shortcut-keysに含まれている文字だと不具合がでるので注意。"
  :type 'char
  :group 'yafastnav)

(defcustom yafastnav-shortcut-keys
     '(
       ?a ?s ?d ?f ?g ?h ?k ?l
       ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
       ?z ?x ?c ?v ?b ?n ?m
       ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
       ?A ?S ?D ?F ?G ?H ?K ?L
       ?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P
       ?Z ?X ?C ?V ?B ?N ?M
       ?, ?. ?: ?- ?^ ?;

       ;; 押しにくいので、使わない
       ;; ?< ?> ?@ ?\* ?\[ ?\]
       ;; ?\\ ?\  ?' ?( ?) ?=
       ;; ?~ ?| ?{ ?} ?\_
       ;; ?! ?\" ?# ?$ ?% ?&
       )
     "要素の選択用ショートカットキーリスト"
     :type 'string
     :group 'yafastnav)

(defface yafastnav-shortcut-key-face-type
  '((((class color)) (:foreground "LightPink" :background "gray15"))
    (t ()))
  "ショートカットキーの表示用フェース型"
  :group 'yafastnav
  )

(defcustom yafastnav-shortcut-key-face 'yafastnav-shortcut-key-face-type
  "ショートカットキーの表示用フェース"
  :type 'face
  :group 'yafastnav
  )

;;;-------------------------------
;;; functions 
;;;-------------------------------
(defun yafastnav-jump-to-current-screen ()
  "現在の画面内の候補へのジャンプ"
 (interactive)
 (let ((top) (bottom))
   (save-excursion
     (move-to-window-line -1)
     (setq bottom (point))
     (move-to-window-line 0)
     (setq top (point))
     )
   (yafastnav-jump-to-between-point top bottom nil)))

(defun yafastnav-jump-to-forward ()
  "現在の画面内のカーソル位置の下の候補へのジャンプ"
 (interactive)
 (let ((top) (bottom))
   (save-excursion
     (setq top (point))
     (move-to-window-line -1)
     (setq bottom (point))
     )
   (yafastnav-jump-to-between-point top bottom nil)))

(defun yafastnav-jump-to-backward ()
  "現在の画面内のカーソル位置の上の候補へのジャンプ"
 (interactive)
 (let ((top) (bottom))
   (save-excursion
     (setq bottom (point))
     (move-to-window-line 0)
     (setq top (point))
     )
   (yafastnav-jump-to-between-point top bottom nil)))

(defun yafastnav-jump-to-between-point (top bottom backward)
  "候補の作成とジャンプの実行"
 (let ((ret)
       (ls nil)
       (ols nil)
       (index 0)
       (start-pos (point))
       (end-pos nil)
       (char nil)
       (regex (assoc major-mode yafastnav-mode-regex-alist)))
   (if (eq regex nil)
     (setq regex yafastnav-default-regex)
     (setq regex (cdr regex)))
   (save-excursion
     (setq inhibit-quit t) ;; C-g で中断されないように
     (goto-char top)
     (while
	 (and
	  (nth index yafastnav-shortcut-keys)
	  (if backward
	      (>= (point) bottom)
	    (<= (point) bottom))
	  (if backward
	      (re-search-backward regex bottom 1)
	    (re-search-forward regex bottom 1)))
       (save-excursion
	 (goto-char (match-beginning 0))
	 (add-to-list 'ls
		      (list
		       (nth index yafastnav-shortcut-keys)
		       (point)))
	 (let ((ov (make-overlay (point) (1+ (point)))))
           (overlay-put ov 'before-string
			(propertize
			 (char-to-string
			  (nth index yafastnav-shortcut-keys))
			 'face yafastnav-shortcut-key-face))
	   (overlay-put ov 'window (selected-window))
           (overlay-put ov 'width 1)
           (overlay-put ov 'priority 100)
	   (add-to-list 'ols ov)
           )
         (setq index (1+ index))
	 ))
     (setq end-pos (point))
     (goto-char start-pos))
   (if (> index 0)
     (progn
       (setq char (read-event "jump to?:"))
       (if (eq char yafastnav-more-shortcutkey)
	   (progn
	     (dolist (o ols) (delete-overlay o))
	     (yafastnav-jump-to-between-point end-pos bottom backward))
	 (unless (eq (assoc char ls) nil)
	   (progn
	     (dolist (o ols)
	       (delete-overlay o))
	     (goto-char (nth 1 (assoc char ls)))))))
       (message "none candidate."))
 (dolist (o ols)
   (delete-overlay o))))

(provide 'yafastnav)
