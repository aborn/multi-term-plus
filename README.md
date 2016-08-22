# multi-term-plus
File [multi-term.el](./multi-term.el) fork from [multi-term](https://www.emacswiki.org/emacs/download/multi-term.el). 
And I do some extension work in [multi-term-plus.el](./multi-term-plus.el).

## Why for this?
[multi-term-plus.el](./multi-term-plus.el) do some extensions for multi-term.

## How to use it?
* Download multi-term-plus files via curl:
```
$ sh -c "$(curl -fsSL https://raw.github.com/aborn/multi-term-plus/master/scripts/install.sh)"
```
* Add following code to your emacs init file:
```
(add-to-list 'load-path "~/multi-term-plus")
(require 'multi-term-config)
```
For detail config, please ref [multi-term-config.el](./multi-term-config.el)

## Extensions
* fast switch when you open multi-terms.  
![](images/find.png "multi-term-find.")  
```elisp
multi-term-find
```
* Smart kill-line in multi-term mode.  
```elisp
multi-term-kill-line
```
* Auto recover previous term buffers when emacs reopen.  

English version readme ends here. Chinese readme provided as follows.

--------------------------------------------------------------------------------
# multi-term-plus
multi-term.el这个文件是从[multi-term](https://www.emacswiki.org/emacs/download/multi-term.el)fork而来. 
在这个基础上，我自己添加了一些新的功能在[multi-term-plus.el](./multi-term-plus.el)里。

## 为什么要做这个工作？
原生的multi-term.el功能不是很完善，像C-e C-a C-k这些功能也不够智能，所以才有了multi-term-plus.el

## 功能点
* 快速切换到不同的multi-terms（绑定到**C-{**这个快捷键里）   
![](images/find.png "multi-term-find.")  
```elisp
multi-term-find
```
* 智能的kill-line操作  
```elisp
multi-term-kill-line
```
* 再次打开emacs时，会自动恢复上一次的几个multi-term，如果不需要这个功能可以通过设置**multi-term-recovery-p**值为nil来关闭该功能。  
```elisp
(setq multi-term-recovery-p nil)
```

