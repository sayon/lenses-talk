MARKDOWN_FILE=lenses.md

all: sources html

sources:
	sed -e '/```haskell/,/```/! s/^/-- /' $(MARKDOWN_FILE) > lenses.hs

html:
	ruby ./mktohtml.rb $(MARKDOWN_FILE) > lenses.html	
	#sed -n -e '/```haskell/,/```/ {/```haskell/ n ; /```/ ! p}' $(MARKDOWN_FILE) > lenses.hs

