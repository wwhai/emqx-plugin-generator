package main

import (
	"fmt"
	"text/template"
	"os"
	"strings"
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
		"etc/emqx_plugin_template.conf",
		"priv/emqx_plugin_template.schema",
		"src/emqx_plugin_template_cli.erl",
		"src/emqx_plugin_template.app.src",
		"src/emqx_plugin_template.erl",
		"src/emqx_plugin_template_app.erl",
		"src/emqx_plugin_template_sup.erl",
		"test/emqx_plugin_template_SUITE.erl",
		"Makefile",
		"rebar.config",
		"LICENSE",
		"README.md",
	}
	distPath := "./dist/" + newPluginName
	os.RemoveAll(distPath)
	for _, fileName := range templates {
		templ, err := template.ParseFiles(fileName)
		if err != nil {
			panic(err)
		}
		type Plugin struct {
			PluginName string
		}

		os.MkdirAll(distPath+"/etc", 755)
		os.MkdirAll(distPath+"/src", 755)
		os.MkdirAll(distPath+"/priv", 755)
		os.MkdirAll(distPath+"/test", 755)

		destFile, err := os.OpenFile(distPath+"/"+strings.Replace(fileName, "template", newPluginName, 1), os.O_CREATE, 0755)
		if err != nil {
			panic(err)
		}
		defer destFile.Close()
		templ.Execute(destFile, &Plugin{newPluginName})
	}

}
