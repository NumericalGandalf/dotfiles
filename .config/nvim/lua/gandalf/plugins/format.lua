local M = { "stevearc/conform.nvim" }

M.manual = false
M.timeout = 2500

function M.invoke()
  require("conform").format({
    lsp_fallback = false,
    async = false,
    timeout_ms = M.timeout,
  })
end

function M.config()
  require("conform").setup({
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "isort", "black" },
      sh = { "shfmt" },
      zsh = { "shfmt" },
      c = { "clang_format" },
      cpp = { "clang_format" },
      json = { "jq" },
      jsonc = { "jq" },
    },
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    group = require("gandalf").gandalfs,
    callback = function()
      if not M.manual then
        M.invoke()
      end
    end,
  })
end

return M