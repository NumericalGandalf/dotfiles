vim.g.mapleader = " "

vim.opt.termguicolors = true

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 15

vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.wrap = false

vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4

vim.opt.swapfile = false
vim.opt.backup = false

vim.opt.incsearch = true
vim.opt.hlsearch = false

vim.opt.fillchars = "eob: "

vim.keymap.set('v', '<C-j>', ":m '>+1<CR>gv=gv")
vim.keymap.set('v', '<C-k>', ":m '<-2<CR>gv=gv")

vim.g.netrw_winsize = 20
vim.keymap.set('n', '<leader>ft', function() vim.cmd(":Lexplore") end)
