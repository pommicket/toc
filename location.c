static bool location_after(Location a, Location b) { /* a is after b? */
	assert(a.ctx == b.ctx);
	return a.code > b.code;
}
