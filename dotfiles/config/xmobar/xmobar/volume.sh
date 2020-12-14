VOLUME=$(pactl list sinks | grep Volume | head -n1 | awk '{print $5}')
printf "\\uF028  "
echo $VOLUME
