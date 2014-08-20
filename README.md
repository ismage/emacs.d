emacs.d
=======

## Caskのインストールを行っておく  
```
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
# caskのパス追加(.bashrcに書いてsource ~/.bashrcする)
export PATH="~/.cask/bin:$PATH"
```
## Caskを使ってパッケージをインストール
```
cd ~/.emacs.d
cask
```

## w3m
```
# caskでw3mのインストールは終わっているが
# w3mコマンドは別途入れる必要がある
brew install w3m
```
