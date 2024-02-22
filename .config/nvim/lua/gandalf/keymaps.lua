local M = {}

local function keymaps()
  local opts = { noremap = true }
  vim.keymap.set("v", "<C-j>", ":m '>+1<CR>gv=gv", opts)
  vim.keymap.set("v", "<C-k>", ":m '<-2<CR>gv=gv", opts)
end

function M.setup()
  keymaps()
end

return M
