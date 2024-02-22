local M = {}

local function settings()
  vim.g.mapleader = " "
  vim.opt.mouse = ""

  vim.opt.termguicolors = true

  vim.opt.number = true
  vim.opt.relativenumber = true
  vim.opt.scrolloff = 15

  vim.opt.autoindent = true
  vim.opt.smartindent = true
  vim.opt.expandtab = true

  vim.opt.wrap = true
  vim.opt.linebreak = true

  vim.opt.swapfile = false
  vim.opt.backup = false

  vim.opt.incsearch = true
  vim.opt.hlsearch = false

  vim.opt.laststatus = 3
  vim.g.netrw_banner = 0

  vim.opt.splitright = true
  vim.opt.wildoptions = "fuzzy,pum,tagfile"

  vim.fn.sign_define("DiagnosticSignError", {
    text = "E",
    texthl = "DiagnosticSignError",
  })
  vim.fn.sign_define("DiagnosticSignWarn", {
    text = "W",
    texthl = "DiagnosticSignWarn",
  })
  vim.fn.sign_define("DiagnosticSignInfo", {
    text = "I",
    texthl = "DiagnosticSignInfo",
  })
  vim.fn.sign_define("DiagnosticSignHint", {
    text = "H",
    texthl = "DiagnosticSignHint",
  })
end

function M.setup()
  settings()
end

return M
