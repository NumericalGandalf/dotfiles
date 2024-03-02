local M = {}
require("gandalf.keymaps").setup()

local function keymaps()
  local opts = { noremap = true }

  vim.keymap.set("v", "<C-j>", ":m '>+1<CR>gv=gv", opts)
  vim.keymap.set("v", "<C-k>", ":m '<-2<CR>gv=gv", opts)

  vim.keymap.set("n", "<leader>bp", ":bprev<CR>", opts)
  vim.keymap.set("n", "<leader>bn", ":bnext<CR>", opts)

  vim.keymap.set("c", "<C-a>", "<Home>", opts)
  vim.keymap.set("c", "<C-e>", "<End>", opts)
  vim.keymap.set("c", "<C-f>", "<C-Right>", opts)
  vim.keymap.set("c", "<C-b>", "<C-Left>", opts)
end

function M.setup()
  keymaps()
end

return M
