/*
  Copyright (C) 2019 Leo Tenenbaum.
  This file is part of toc. toc is distributed under version 3 of the GNU General Public License, without any warranty whatsoever.
  You should have received a copy of the GNU General Public License along with toc. If not, see <https://www.gnu.org/licenses/>.
*/
static U32 rand_u32(U32 seed) {
	U64 seed64 = (U64)seed;
	return (U32)((seed64 * 0x832f0fda4e1a8642 + 0x41d49cd5459a2ab4) >> 32);
}
