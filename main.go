package main

import (
	"fmt"
	"os"
	"strings"
	"text/template"
)

func main() {

	args := os.Args
	if len(args) != 2 {
		fmt.Println("-----------------------------------------------------------")
		fmt.Println("%   Usage: newplugin {name}, like 'newplugin demo_plugin'")
		fmt.Println("-----------------------------------------------------------")
		return
	}
	newPluginName := strings.ToLower(args[1])
	templates := []string{
		".tpl/etc/emqx_plugin_template.conf",
		".tpl/priv/emqx_plugin_template.schema",
		".tpl/src/emqx_plugin_template_cli.erl",
		".tpl/src/emqx_plugin_template.app.src",
		".tpl/src/emqx_plugin_template.erl",
		".tpl/src/emqx_plugin_template_app.erl",
		".tpl/src/emqx_plugin_template_sup.erl",
		".tpl/test/emqx_plugin_template_SUITE.erl",
		".tpl/Makefile",
		".tpl/rebar.config",
		".tpl/LICENSE",
		".tpl/README.md",
	}
	distPath := "./dist/emqx_" + newPluginName+"_plugin"
	os.RemoveAll(distPath)
	for _, fileName := range templates {
		t, err := template.ParseFiles(fileName)
		if err != nil {
			fmt.Println("插件生成失败，错误信息:", err.Error())
			return
		}
		type Plugin struct {
			PluginName string
		}

		fmt.Println("正在创建插件目录结构")
		var permission os.FileMode = 755
		os.MkdirAll(distPath+"/etc", permission)
		os.MkdirAll(distPath+"/src", permission)
		os.MkdirAll(distPath+"/priv", permission)
		os.MkdirAll(distPath+"/test", permission)
		fmt.Println("创建插件目录结构完成")

		distFileName := strings.Replace(strings.Replace(fileName, "template", newPluginName, 1), ".tpl", "", 1)
		destFile, err := os.OpenFile(distPath+"/"+distFileName, os.O_CREATE, permission)
		if err != nil {
			fmt.Println("插件生成失败,错误信息:", err.Error())
			return
		}
		defer destFile.Close()
		fmt.Println("文件生成中:", destFile.Name())
		t.Execute(destFile, &Plugin{newPluginName})
		fmt.Println("文件生成完成:", destFile.Name())

	}
	fmt.Println("||")
	fmt.Println("|| 插件生成完成, 位于:", distPath)
	fmt.Println("||")
}
