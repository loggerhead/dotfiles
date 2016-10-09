#!/bin/bash
if [ `tmux show -vg mouse` = "on" ]; then
    tmux set -g mouse off
else
    tmux set -g mouse on
    tmux bind -n WheelUpPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if -Ft= '#{pane_in_mode}' 'send-keys -M' 'select-pane -t=; copy-mode -e; send-keys -M'"
    tmux bind -n WheelDownPane select-pane -t= \; send-keys -M
    tmux bind -n C-WheelUpPane select-pane -t= \; copy-mode -e \; send-keys -M
    tmux bind -t vi-copy    C-WheelUpPane   halfpage-up
    tmux bind -t vi-copy    C-WheelDownPane halfpage-down
    tmux bind -t emacs-copy C-WheelUpPane   halfpage-up
    tmux bind -t emacs-copy C-WheelDownPane halfpage-down
fi
