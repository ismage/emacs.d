emacs.d
=======
Caskをやめてel-getに変更

初回起動時に必要なパッケージをインストール
el-get-bundleに!をつけるとインストールとrequireを行う

```
# hogeパッケージをrequireした状態にしてくれる
(el-get-bundle! hoge)
```

