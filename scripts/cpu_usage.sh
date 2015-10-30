#! /bin/bash

cpu_usage=$(top -bn1 | grep "Cpu" | \
	sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | \
	awk '{print $4"%"}' | \
	grep -o [0-9]* | awk '{print}' ORS=' ' | \
	awk '{print ($1 + $2)/2, "%"}'
	)

echo "$cpu_usage"

