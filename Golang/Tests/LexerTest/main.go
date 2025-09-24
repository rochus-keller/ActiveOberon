// main.go - Test program for ActiveOberon Lexer
// This program reads an Oberon source file and prints all tokens found by the lexer

package main

import (
	"fmt"
	"os"
	"strings"
	"path/filepath"
	ao "github.com/rochus-keller/ActiveOberon/Golang/ActiveOberon"
)

func main() {
	// Check command line arguments
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s <oberon-file.Mod>\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "Example: %s TestModule.Mod\n", os.Args[0])
		os.Exit(1)
	}

	filename := os.Args[1]

	// Check if file exists and has .Mod extension
	if _, err := os.Stat(filename); os.IsNotExist(err) {
		fmt.Fprintf(os.Stderr, "Error: File '%s' does not exist\n", filename)
		os.Exit(1)
	}

	ext := filepath.Ext(filename)
	if ext != ".Mod" && ext != ".mod" {
		fmt.Fprintf(os.Stderr, "Warning: File '%s' does not have .Mod extension\n", filename)
	}

	// Create lexer and set up file input
	lexer := ao.NewLexer()
	lexer.SetIgnoreComments(false) // We want to see comments in output

	err := lexer.SetStreamFromFile(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file '%s': %v\n", filename, err)
		os.Exit(1)
	}

	fmt.Printf("Tokenizing file: %s\n", filename)
	fmt.Printf("%-12s %-12s %s\n", "Position", "Token", "Value")
	fmt.Printf("%-12s %-12s %s\n", "--------", "-----", "-----")

	// Tokenize the entire file
	tokenCount := 0
	for {
		token := lexer.NextToken()

		if token.Type == ao.TokEof {
			fmt.Printf("%-6d%-5d %-12s\n", token.LineNr, token.ColNr,ao.TokenTypeName(token.Type))
			break
		}

		if token.Type == ao.TokInvalid {
			fmt.Printf("%-6d%-5d %-12s ERROR: %s\n", token.LineNr, token.ColNr, ao.TokenTypeName(token.Type), string(token.Val))
			break
		}

		// Print the token in a nice format
		tokenName := ao.TokenTypeName(token.Type)
		value := string(token.Val)

		if len(value) == 0 {
			fmt.Printf("%-6d%-5d %s\n", token.LineNr, token.ColNr, tokenName)
		} else {
			// Escape special characters for display
			displayValue := value
			if token.Type == ao.TokString {
				displayValue = fmt.Sprintf("%s", value)
			} else if token.Type == ao.TokComment {
				// Show first line of comment only
				lines := strings.Split(value, "\n")
				if len(lines) > 1 {
					displayValue = fmt.Sprintf("%s... (%d lines)", lines[0], len(lines))
				} else {
					displayValue = value
				}
			}
			fmt.Printf("%-6d%-5d %-12s %s\n", token.LineNr, token.ColNr, tokenName, displayValue)
		}

		tokenCount++
	}

	fmt.Printf("\nTokenization complete. Total tokens: %d\n", tokenCount)
	fmt.Printf("Source lines of code (SLOC): %d\n", lexer.GetSloc())
}
