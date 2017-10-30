# Ctrlnum

## tl;dr

Conveniently switch between file buffers just like you would with
Google Chrome's tabs default key bindings:

`C-1` switches to the first file buffer

`C-2` switches to the second file buffer

...

`C-9` switches to the 9th file buffer

`C-0` switches to the 10th file buffer

Because the native Emacs buffer-list is kept intact, you can still use all
the other buffer switching tactics you already have.

## Demo

![ctrlnum demo](https://github.com/marcofognog/ctrlnum/blob/master/demo.gif)

### Explanation Cucumber style

```
Given I have file buffers and buffers without files,
And I cycle through all the buffers including buffers without files,
When I can cycle forward only the file buffers with `ctrlnum-next` function
And also go to the second file buffer with `C-2` binding
And I can know `C-2` will always switch to the same file, until change that
```


## Installation

Just copy ctrlnum.el and require it in your configuration file. No packages available yet.

If you want to cicle through and reorder the files with exactly the same binding as the browser:

```
(global-set-key (kbd "C-<next>") 'ctrlnum-next)
(global-set-key (kbd "C-<prior>") 'ctrlnum-previous)
(global-set-key [\C-\S-prior] 'ctrlnum-switch-order-prev)
(global-set-key [\C-\S-next] 'ctrlnum-switch-order-next)
```

### Gotcha to watch for
Evil mode users may want to add the following line your config:

`(define-key evil-motion-state-map (kbd "C-6") nil)`

That will free up the key binding `C-6`. Because evil-mode uses for a buffer switching command, that may cause some confusion.