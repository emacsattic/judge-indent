# judge-indent.el
`judge-indent-mode' は、ファイルを開いた瞬間、
以下の 9 (厳密には 7) パターンの中から
インデント幅およびタブ幅を自動的に判定します。
既存のインデントスタイルを乱さずに、
他人・チームのコードに手を入れることが可能となります。

      \  indent
       \  2 4 8
    tab \------
      2 | U - -
      4 | X U -
      8 | X X U <- インデント幅＝タブ幅のときはそれ以上の判定を行わない。
    nil | X X X

# 使い方
以下の 3 行を emacs の設定ファイルに追加してください。

    (require 'judge-indent)
    (global-judge-indent-mode t)
    (setq judge-indent-major-modes '(c-mode python-mode sh-mode))

# カスタマイズ
デフォルトのインデント幅 (2 か 4 か 8) を設定する。
デフォルト: `c-basic-offset' のデフォルト値、もしくは 4。

    (setq judge-indent-default-indent-width 2)

デフォルトのタブ幅 (2 か 4 か 8) を設定する。
デフォルト: `tab-width' のデフォルト値、もしくは 8。

    (setq judge-indent-default-tab-width 4)

インデントがあまり深くないときにタブを好むかどうかのフラグを設定する。
デフォルト: `indent-tabs-width' のデフォルト値、もしくは nil。

    (setq judge-indent-prefer-tabs-mode t)

インデント幅とタブ幅を判定するときに用いる相対許容誤差 [%] を設定する。
デフォルト: 5 %。

    (setq judge-indent-relative-tolerance 3)

大きいサイズのファイルに対して検索リミットを設定する。
デフォルト: 30000 文字 (＝約 1000 行)。

    (setq judge-indent-search-limit 60000)

# 関数
* judge-indent-mode

マイナーモードをオン／オフにする。

* judge-indent-buffer
* judge-indent-region

バッファ／リージョンからインデント幅およびタブ幅を判定する。

* judge-indent-set-indent-tab-widths
* judge-indent-set-indent-width{2, 4, 8}-disable-tab
* judge-indent-set-indent-width{2, 4, 8}-tab-width{2, 4, 8}

インデント幅およびタブ幅を手動で設定する。

* judge-indent-set-indent-width
* judge-indent-set-indent-width{2, 4, 8}

インデント幅を手動で設定する。

* judge-indent-set-tab-width
* judge-indent-disable-tab
* judge-indent-set-tab-width{2, 4, 8}

タブ幅を手動で設定する。

* judge-indent-message-indent-counts-buffer
* judge-indent-message-indent-counts-region

バッファ／リージョン内のインデントをカウントして表示する。

# バージョン
## 1.1.0 on 2 July 2011

精度向上。リージョンに関する関数。リファクタリング。

## 1.0.0 on 26 June 2011

ファーストコミット。

# 既知の問題
