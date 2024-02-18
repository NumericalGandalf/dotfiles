local M = { "nvim-treesitter/nvim-treesitter" }

M.build = ":TSUpdate"

M.lazy = false

function M.config()
  require("nvim-treesitter.configs").setup({
    auto_install = true,
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = false,
    },
  })

  vim.treesitter.language.register("bash", "zsh")
end

return M
