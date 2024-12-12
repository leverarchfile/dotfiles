-- toggle file explorer
vim.keymap.set("n", "<leader>ee", "<cmd>NvimTreeToggle<CR>", { desc = "Toggle file explorer" }) 
-- focus file explorer
vim.keymap.set("n", "<leader>ef", "<cmd>NvimTreeFocus<CR>", { desc = "Open the tree if it is closed, and then focus on the tree" }) 
-- toggle file explorer on current file
-- vim.keymap.set("n", "<leader>ef", "<cmd>NvimTreeFindFileToggle<CR>", { desc = "Toggle file explorer on current file" }) 
-- collapse file explorer
vim.keymap.set("n", "<leader>ec", "<cmd>NvimTreeCollapse<CR>", { desc = "Collapse file explorer" }) 
-- refresh file explorer
vim.keymap.set("n", "<leader>er", "<cmd>NvimTreeRefresh<CR>", { desc = "Refresh file explorer" }) 

require("nvim-tree").setup({
	view = {
		width = 35,
    relativenumber = true,
	},
	renderer = {
		indent_markers = {
    enable = true,
		},
	},
	-- disable window_picker for explorer to work well with window splits
	actions = {
		open_file = {
			window_picker = {
				enable = false,
			},
		},
	},
})
