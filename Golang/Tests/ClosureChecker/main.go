package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	ao "github.com/rochus-keller/ActiveOberon/Golang/ActiveOberon"
)

func splitFilesAndDirs(args []string) (files []string, dirs []string) {
	for _, a := range args {
		info, err := os.Stat(a)
		if err != nil {
			// not found; treat as pattern or report later
			continue
		}
		if info.IsDir() {
			dirs = append(dirs, a)
		} else {
			files = append(files, a)
		}
	}
	return files, dirs
}

func listModInDir(dir string) ([]string, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, err
	}
	var out []string
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		if strings.EqualFold(filepath.Ext(e.Name()), ".mod") {
			out = append(out, filepath.Join(dir, e.Name()))
		}
	}
	return out, nil
}

type ModuleBase struct {
	modules []*ao.Declaration
	model   *ao.AstModel
}

func (self *ModuleBase) GetModules() []*ao.Declaration {
	return self.modules
}

func (self *ModuleBase) AddModule(m *ao.Declaration) {
	self.modules = append(self.modules, m)
}

func (self *ModuleBase) LoadModule(imp *ao.Import) *ao.Declaration {

	modName := string(imp.ModuleName)
	if modName == "SYSTEM" {
		return self.model.SYSTEM
	}

	for _, mod := range self.modules {
		if string(mod.Name) == modName {
			return mod
		}
	}
	return nil
}

func (self *ModuleBase) Validate(module *ao.Declaration) bool {
	if module.Validated {
		return true
	}
	v := ao.NewValidator(self.GetModel(), self, false)
	v.Validate(module, nil)
	module.Validated = true
	if len(v.Errors) != 0 {
		fmt.Printf("validation of module '%s' had FAILED with %d error(s):\n", string(module.Name), len(v.Errors))
		for i, e := range v.Errors {
			pos := e.Pos.String()
			if e.Path != "" {
				pos = fmt.Sprintf("%s:%s", strings.TrimSuffix(filepath.Base(e.Path), ".Mod"), pos)
			}
			fmt.Printf("    %2d) %s at %s\n", i+1, e.Msg, pos)
			if i == 9 {
				break
			}
		}
		return false
	}
	return true
}

func (self *ModuleBase) GetModel() *ao.AstModel {
	if self.model == nil {
		self.model = ao.NewAstModel()
	}
	return self.model
}

func main() {
	flag.Parse()

	files, dirs := splitFilesAndDirs(flag.Args())

	for _, dir := range dirs {
		mods, err := listModInDir(dir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "scan %s: %v\n", dir, err)
			continue
		}
		files = append(files, mods...)
	}

	if len(files) == 0 {
		fmt.Println("No .Mod files provided. Nothing to do.")
		return
	}

	success := 0
	failed := 0

	var modules ModuleBase

	for _, f := range files {
		// Validate extension
		ext := strings.ToLower(filepath.Ext(f))
		if ext != ".mod" {
			fmt.Printf("Skipping %s (not a .Mod file)\n", f)
			continue
		}

		// Announce parse
		fmt.Printf("Parsing: %s\n", f)

		// Open lexer
		lexer := ao.NewLexer()
		lexer.SetIgnoreComments(true)
		if err := lexer.SetStreamFromFile(f); err != nil {
			fmt.Printf("  Error opening file: %v\n", err)
			failed++
			continue
		}

		parser := ao.NewParser(modules.GetModel(), lexer)
		parser.RunParser()

		// Collect errors (Go 1.21 compatible)
		errs := parser.Errors()
		if len(errs) > 0 {
			fmt.Printf("  FAILED with %d error(s):\n", len(errs))
			failed++
		} else {
			// Success summary: show module name if available
			mod := parser.TakeResult()
			modules.AddModule(mod)
			success++
		}
	}

	// Final summary
	fmt.Println()
	fmt.Printf("Summary: %d succeeded, %d failed\n", success, failed)

	fmt.Println("=== Closure Conversion Analysis ===")

	for _, mod := range modules.GetModules() {
		if modules.Validate(mod) {
			/*
				analyzer := ao.NewClosureAnalyzerForModule(mod)
				results := analyzer.Analyze()
				fmt.Printf("module %s ", string(mod.Name))
				analyzer.PrintResults()

				for _, info := range results {
					// Process each nested procedure's closure requirements
					_ = info
				}
			*/
		} else {
			failed++
		}
	}

	// Exit code: non-zero if any failures
	if failed > 0 {
		os.Exit(1)
	}
}
