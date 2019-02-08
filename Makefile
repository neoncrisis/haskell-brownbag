.PHONY: all
all:
	@echo "Please specify a target!"

.PHONY: watch
watch:
	@which ghcid > /dev/null || stack install ghcid
	@ghcid \
		--command "stack ghci brownbag:exe:brownbag" \
		--restart "stack.yaml" \
		--restart "package.yaml" \
		--restart ".ghci" \
		--test ":main" \
		$(GHCID_OPTS)
