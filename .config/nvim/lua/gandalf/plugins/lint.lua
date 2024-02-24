local M = { "mfussenegger/nvim-lint" }

M.event = { "LspAttach", "BufWritePost" }

function M.config()
  local lint = require("lint")

  lint.linters_by_ft = {
    -- lua = { "luacheck" },
    python = { "flake8", "mypy" },
  }

  vim.api.nvim_create_autocmd({ "LspAttach", "BufWritePost" }, {
    group = vim.api.nvim_create_augroup("GandalfAulint", { clear = true }),
    callback = function()
      lint.try_lint()
    end,
  })
end

return M
