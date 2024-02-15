local M = { "neovim/nvim-lspconfig" }

M.dependencies = {
  "folke/neodev.nvim",
  "williamboman/mason-lspconfig.nvim",
  "hrsh7th/cmp-nvim-lsp",
  "stevearc/conform.nvim",
  "mfussenegger/nvim-lint",
}

M.event = "BufEnter"

local function attach_generic(opts)
  local telescope = require("telescope.builtin")

  vim.keymap.set("n", "gr", telescope.lsp_references, opts)
  vim.keymap.set("n", "gi", telescope.lsp_implementations, opts)
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
  vim.keymap.set("n", "gd", telescope.lsp_definitions, opts)
  vim.keymap.set("n", "gt", telescope.lsp_type_definitions, opts)

  vim.keymap.set("n", "gc", telescope.lsp_outgoing_calls, opts)
  vim.keymap.set("n", "gC", telescope.lsp_incoming_calls, opts)

  vim.keymap.set("n", "gS", telescope.lsp_document_symbols, opts)
  vim.keymap.set("n", "gs", telescope.lsp_workspace_symbols, opts)

  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)

  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)

  vim.keymap.set("n", "<leader>vd", vim.diagnostic.setqflist, opts)
  vim.keymap.set("n", "<leader>vD", vim.diagnostic.setloclist, opts)

  vim.keymap.set("n", "gh", function()
    vim.cmd(":ClangdSwitchSourceHeader")
  end, opts)
end

function M.config()
  require("neodev").setup()
  require("mason-lspconfig").setup()

  vim.api.nvim_create_autocmd("LspAttach", {
    group = require("gandalf").gandalfs,
    callback = function(ev)
      local opts = { buffer = ev.buf }
      vim.bo[opts.buffer].omnifunc = "v:lua.vim.lsp.omnifunc"
      attach_generic(opts)
    end,
  })

  local lspconfig = require("lspconfig")
  require("lspconfig.ui.windows").default_options = {
    border = "rounded",
  }

  local capabilities = require("cmp_nvim_lsp").default_capabilities()
  local handlers = {
    ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" }),
    ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" }),
  }

  lspconfig.lua_ls.setup({
    capabilities = capabilities,
    handlers = handlers,
  })

  lspconfig.clangd.setup({
    capabilities = capabilities,
    handlers = handlers,
  })

  lspconfig.bashls.setup({
    capabilities = capabilities,
    handlers = handlers,
    filetypes = { "sh", "zsh" },
    settings = {
      bashIde = {
        globPattern = "*@(.sh|.inc|.bash|.zsh.|command)",
      },
    },
  })

  lspconfig.pyright.setup({
    capabilities = capabilities,
    handlers = handlers,
  })

  lspconfig.jsonls.setup({
    capabilities = capabilities,
    handlers = handlers,
  })
end

return M
