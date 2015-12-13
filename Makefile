MARKDOWN_FILE=lenses.md

all: sources html launch

sources:
	sed 's/^-- \?//g' < lenses.hs > $(MARKDOWN_FILE)
	#sed -e '/```haskell/,/```/ ! s/^/-- / ' $(MARKDOWN_FILE) | sed -e 's/^```/-- ```/' > lenses.hs

html:
	ruby ./mktohtml.rb $(MARKDOWN_FILE) > lenses.html	
	#sed -n -e '/```haskell/,/```/ {/```haskell/ n ; /```/ ! p}' $(MARKDOWN_FILE) > lenses.hs

launch:
	#ghci lenses.hs
