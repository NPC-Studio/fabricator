var prev_timer = timer;
timer += 1.0 / 60.0;

if prev_timer < 1.0 && timer >= 1.0 {
	room = room2;
} else if timer >= 2.0 {
	room = room1;
	timer = 0.0;
}