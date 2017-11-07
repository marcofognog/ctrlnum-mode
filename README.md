# ctrlnum-mode

## tl;dr

Consistently switch between file buffers just like you would with
Google Chrome's tabs default key bindings:

`C-1` switches to the first file buffer

`C-2` switches to the second file buffer

...

`C-9` switches to the 9th file buffer

`C-0` switches to the 10th file buffer

Because the native Emacs `buffer-list` is kept intact, you can still use all
the other buffer switching tactics you already have.

## Demo

![ctrlnum demo](https://github.com/marcofognog/ctrlnum/blob/master/demo.gif)

### Explanation Cucumber style

```
Given I have file buffers and buffers without files,
By calling `ctrlnum-next` function
I can cycle forward only among the file buffers
When I call `C-2` binding (`ctrlnum-switch-2`)
It switches to the second file buffer
And I know `C-2` will always switch to the same file, until I change that
```

## Functionality

 Function                  | Description                                       |
---------------------------|---------------------------------------------------|
 ctrlnum-switch-n          | Switch to the nth file buffer                     |
 ctrlnum-next              | Switch to the next (to the right) file buffer     |
 ctrlnum-previous          | Switch to the previous (to the left) file buffer  |
 ctrlnum-switch-order-prev | Rearrange the position of the buffer to the left  |
 ctrlnum-switch-order-next | Rearrange the position of the buffer to the right |

## Installation

Just copy ctrlnum.el and require it in your configuration file. No packages available yet.

### Mimicking Chrome's bindings

If you want to cycle through and reorder the files with exactly the same binding as the browser:

```
(global-set-key (kbd "C-<next>") 'ctrlnum-next)
(global-set-key (kbd "C-<prior>") 'ctrlnum-previous)
(global-set-key [\C-\S-prior] 'ctrlnum-switch-order-prev)
(global-set-key [\C-\S-next] 'ctrlnum-switch-order-next)
```

### Gotchas to watch for

Evil mode users may want to add the following line your config:

`(define-key evil-motion-state-map (kbd "C-6") nil)`

That will free up the key binding `C-6`. Because evil-mode uses it for a buffer switching command, that may cause some confusion.