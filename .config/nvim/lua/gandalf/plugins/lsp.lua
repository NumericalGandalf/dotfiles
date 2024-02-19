local M = { "neovim/nvim-lspconfig" }

M.dependencies = {
  "folke/neodev.nvim",
  "williamboman/mason-lspconfig.nvim",
  "hrsh7th/cmp-nvim-lsp",
  "stevearc/conform.nvim",
  "mfussenegger/nvim-lint",
}

M.lazy = false

local function extend_features_tbl(features)
  features = features or {}
  return {
    definitions = features.definitions or true,
    declarations = features.declarations or false,
    refrences = features.refrences or true,
    type_definitions = features.type_definitions or true,
    implementations = features.implementations or false,
    call_hierarchy = features.call_hierarchy or false,
  }
end

local function extend_opts_tbl(opts)
  opts = opts or {}
  return {
    buffer = opts.buffer or vim.api.nvim_get_current_buf(),
    noremap = true,
  }
end

local function on_attach(features, opts)
  features = extend_features_tbl(features)
  opts = extend_opts_tbl(opts)
  local telescope = require("telescope.builtin")

  vim.bo[opts.buffer].omnifunc = "v:lua.vim.lsp.omnifunc"

  if features.definitions then
    vim.keymap.set("n", "gd", telescope.lsp_definitions, opts)
  end

  if features.declarations then
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
  end

  if features.refrences then
    vim.keymap.set("n", "gr", telescope.lsp_references, opts)
  end

  if features.type_definitions then
    vim.keymap.set("n", "gt", telescope.lsp_type_definitions, opts)
  end

  if features.implementations then
    vim.keymap.set("n", "gi", telescope.lsp_implementations, opts)
  end

  if features.call_hierarchy then
    vim.keymap.set("n", "gc", telescope.lsp_outgoing_calls, opts)
    vim.keymap.set("n", "gC", telescope.lsp_incoming_calls, opts)
  end

  vim.keymap.set("n", "gs", telescope.lsp_document_symbols, opts)
  vim.keymap.set("n", "gS", telescope.lsp_workspace_symbols, opts)

  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)

  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)

  vim.keymap.set("n", "<leader>vd", vim.diagnostic.setqflist, opts)
  vim.keymap.set("n", "<leader>vD", vim.diagnostic.setloclist, opts)
end

local function on_attach_luals()
  on_attach()
end

local function on_attach_clangd()
  local opts = extend_opts_tbl()
  on_attach({
    declarations = true,
    implementations = true,
    call_hierarchy = true,
  }, opts)
  vim.keymap.set("n", "gh", function()
    vim.api.nvim_command("ClangdSwitchSourceHeader")
  end, opts)
end

local function on_attach_bashls()
  on_attach({
    type_definitions = false,
  })
end

local function on_attach_vimls()
  on_attach({
    type_definitions = false,
  })
end

function M.config()
  require("neodev").setup()
  require("mason-lspconfig").setup()

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
    on_attach = on_attach_luals,
  })

  lspconfig.clangd.setup({
    capabilities = capabilities,
    handlers = handlers,
    on_attach = on_attach_clangd,
  })

  lspconfig.bashls.setup({
    capabilities = capabilities,
    handlers = handlers,
    on_attach = on_attach_bashls,
    filetypes = { "sh", "zsh" },
    settings = {
      bashIde = {
        globPattern = "*@(.sh|.inc|.bash|.zsh.|command)",
      },
    },
  })

  lspconfig.vimls.setup({
    capabilities = capabilities,
    handlers = handlers,
    on_attach = on_attach_vimls,
  })

  lspconfig.pyright.setup({
    capabilities = capabilities,
    handlers = handlers,
  })
end

return M
