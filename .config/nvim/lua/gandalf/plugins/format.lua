local M = { "stevearc/conform.nvim" }

M.event = "BufWritePre"

function M.config()
  require("conform").setup({
    formatters_by_ft = {
      lua = { "stylua" },
      c = { "clang_format" },
      cpp = { "clang_format" },
      vim = { "vint" },
      sh = { "shfmt" },
      zsh = { "shfmt" },
      python = { "isort", "black" },
      json = { "jq" },
      jsonc = { "jq" },
    },
    format_on_save = {
      timeout_ms = 1000,
      async = false,
      lsp_fallback = false,
    },
  })
end

return M
