NAME=aggregated_check
VERSION=0.0.1

.PHONY: package
package:
	rm -f aggcheck
	mix deps.get
	mix escript.build
	fpm --depends erlang -s dir -t deb -n $(NAME) -v $(VERSION) --prefix /usr/local/bin aggcheck
