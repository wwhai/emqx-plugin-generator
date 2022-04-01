# EMQX 插件项目生成器

## 关于项目
自用脚本，主要用来生成 `emqx` 的插件。

## 使用
```sh
plugingen {插件名}
```

例如：
```sh
plugingen demo_plugin
```

或者自己构建工具:
```sh
go build
```

生成后的插件在 `dist` 目录下,文件结构如下:
```
LICENSE
Makefile
README.md
etc
priv
rebar.config
src
test
```

## 作者
- wwhai

## 社区
- QQ群：475512169
- 博客：https://wwhai.github.io