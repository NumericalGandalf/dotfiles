if not vim.g.IsDef("lua") then
    require("lspconfig")["lua_ls"].setup({
        capabilities = require('cmp_nvim_lsp').default_capabilities(),
        settings = {
            Lua = {
                diagnostics = {
                    globals = { 'vim' },
                },
                completion = {
                    callSnippet = "Replace"
                }
            }
        }
    })
end
