local M = { "williamboman/mason-lspconfig.nvim" }

function M.config()
  require("mason-lspconfig").setup()
end

return M
