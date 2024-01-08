return {
	"williamboman/mason.nvim",
	build = ":MasonUpdate",
	event = "VimEnter",
	config = function()
		require("mason").setup()
	end,
}
