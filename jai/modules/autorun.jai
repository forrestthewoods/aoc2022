// ----
// Metaprogram Code
// ----
get_plugin :: () -> *Plugin {
    p := New(AutorunPlugin);  // We are making this on the heap so you could have more than one instance of this plugin running.

    p.message  = message;
    p.shutdown = shutdown;

    return p;
}

message :: (_p: *Plugin, message: *Compiler.Message) {
    p := cast(*AutorunPlugin) _p;

    if message.kind == .COMPLETE {
        complete := cast(*Compiler.Message_Complete) message;
        if complete.error_code != .NONE {
            p.should_run = false;
        }
    }
}

shutdown :: (_p: *Plugin) {
    p := cast(*AutorunPlugin) _p;

    if p.should_run {
        using options := Compiler.get_build_options(p.workspace);
        if output_type != .EXECUTABLE {
            log("[Autorun] Not running anything, because the Build_Options.output_type of Workspace % is %.\n", p.workspace, output_type);
        } else {
            run_string := ifx output_path then String.join(output_path, output_executable_name, allocator=temp) else output_executable_name;
            log("[Autorun] Running: %\n", run_string);
            Process.run_command(run_string);
        }
    } else {
        log("[Autorun] There were errors, so we are not running.\n");
    }
    
    free(p);
}

#scope_module
#import "Basic";
Compiler :: #import "Compiler";
Process  :: #import "Process";
String   :: #import "String";

Plugin   :: Compiler.Metaprogram_Plugin;

AutorunPlugin :: struct {
    #as using base: Compiler.Metaprogram_Plugin;
    should_run := true;
}