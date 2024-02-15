local M = { "mfussenegger/nvim-lint" }

function M.config()
  local lint = require("lint")
  lint.linters_by_ft = {
    python = { "flake8", "mypy" },
  }

  vim.api.nvim_create_autocmd("BufWritePost", {
    callback = function()
      lint.try_lint()
    end,
  })
end

return M