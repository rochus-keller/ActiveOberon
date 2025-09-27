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

	model := ao.NewAstModel()
	var modules []*ao.Declaration

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

		parser := ao.NewParser(model, lexer)
		parser.RunParser()

		// Collect errors (Go 1.21 compatible)
		errs := parser.Errors()
		if len(errs) > 0 {
			fmt.Printf("  FAILED with %d error(s):\n", len(errs))
			failed++
		} else {
			// Success summary: show module name if available
			mod := parser.TakeResult()
			// TODO v := ao.NewValidator(model, nil, false)
			modules = append(modules, mod)
			success++
		}
	}

	// Final summary
	fmt.Println()
	fmt.Printf("Summary: %d succeeded, %d failed\n", success, failed)

	fmt.Println("=== Closure Conversion Analysis ===")

	for _, mod := range modules {
		analyzer := ao.NewClosureAnalyzerForModule(mod)
		results := analyzer.Analyze()
		fmt.Printf("module %s ", string(mod.Name))
		analyzer.PrintResults()

		for _, info := range results {
			// Process each nested procedure's closure requirements
			_ = info
		}
	}

	// Exit code: non-zero if any failures
	if failed > 0 {
		os.Exit(1)
	}
}
