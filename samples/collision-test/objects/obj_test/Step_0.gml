var mouse_delta_x = mouse_x - last_mouse_x;
var mouse_delta_y = mouse_y - last_mouse_y;

last_mouse_x = mouse_x;
last_mouse_y = mouse_y;

if mouse_check_button(mb_left) {
	if abs(mouse_x - x) <= 32 && abs(mouse_y - y) <= 32 {
		mouse_dragging = true;
	}
} else {
	mouse_dragging = false;
}

if mouse_dragging {
	x += mouse_delta_x;
	y += mouse_delta_y;
}

colliding_1 = collision_line(x - 32, y - 32, x + 32, y + 32, all, false, false) != noone;
colliding_2 = collision_line(x + 32, y - 32, x - 32, y + 32, all, false, false) != noone;