local M = {}

local function ensure_lazy()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable",
      lazypath,
    })
  end
  vim.opt.rtp:prepend(lazypath)
end

local function lazy_opts()
  return {
    defaults = {
      lazy = true,
    },
    ui = {
      border = "rounded",
    },
    checker = {
      enabled = false,
    },
    change_detection = {
      enabled = false,
      notify = false,
    },
    profiling = {
      loader = true,
      require = true,
    },
  }
end

function M.setup()
  ensure_lazy()
  require("lazy").setup("gandalf.plugins", lazy_opts())
end

return M
