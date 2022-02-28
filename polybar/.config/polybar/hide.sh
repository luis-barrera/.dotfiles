#! bin/bash

case "$(pidof polybar | wc -w)" in
0)
	polybar topbar 2>&1 | tee -a /tmp/polybar1.log & disown
	;;
*)
	killall -q polybar
	;;
# *)
# 	echo "Ayuda"
# 	;;
esac
