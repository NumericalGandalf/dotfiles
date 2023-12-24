if not vim.g.IsDef("python") then
    require("lspconfig")["pyright"].setup({
        capabilities = require('cmp_nvim_lsp').default_capabilities()
    })
    require("dap").adapters.python = function(cb, config)
        if config.request == 'attach' then
            local port = (config.connect or config).port
            local host = (config.connect or config).host or '127.0.0.1'
            cb({
                type = 'server',
                port = assert(port, '`connect.port` is required for a python `attach` configuration'),
                host = host,
                options = {
                    source_filetype = 'python',
                },
            })
        else
            cb({
                type = 'executable',
                command = '/usr/bin/python',
                args = { '-m', 'debugpy.adapter' },
                options = {
                    source_filetype = 'python',
                },
            })
        end
    end
    require("dap").configurations.python = {
        {
            type = "python",
            request = "launch",
            name = "Launch Entry (main.py)",
            program = vim.fn.getcwd() .. "/src/main.py",
            pythonPath = "/usr/bin/python"
        }
    }
end
