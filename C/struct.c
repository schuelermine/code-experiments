// WARNING! THIS DOESN’T WORK!

#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>

struct Invocation {
    char* executable_name;
    char** arguments;
};

struct RunningProcess {
    char* executable_path;
    char* *root_path;
    struct Invocation invocation;
    char** *environment_variables;
    char* *working_directory;
    int process_id;
};

struct InvocationResult {
    int *process_id;
    bool success;
};

struct InvocationResult invocationFailure = {NULL, false};

struct InvocationResult invocationSuccess(int process_id) {
    struct InvocationResult result = {&process_id, true};
    return result;
}

struct InvocationResult invoke_process(struct Invocation invocation) {
    printf("Attempted to launch process %s", invocation.executable_name);
    return invocationFailure;
}

struct InvocationResult reinvoke_process(struct RunningProcess process) {
    struct Invocation invocation = process.invocation;
    return invoke_process(invocation);
}

struct RunningProcess get_running_process() {
    char* args[2] = {"-l", NULL};
    struct Invocation invocation = {
        "bash",
        args
    };
    char* environment_variables[1] = {NULL};
    char root_path[2] = "/";
    struct RunningProcess result = {
        "/usr/bin/bash",
        &root_path,
        invocation,
        &environment_variables,
        &root_path,
        111
    };
    return result;
}

// takes a null-terminated string array
void print_string_array(char** array) {
    printf("{");
    while (*array != NULL) {
        printf("\"%s\"", *array);
        array++;
        if (*array != NULL)
            printf(", ");
    }
    printf("}");
}

void print_invocation(struct Invocation invocation) {
    printf("{\n");
    printf("\t%s,\n", invocation.executable_name);
    printf("\t");
    print_string_array(invocation.arguments);
    printf("\n}");
}

int main() {
    struct RunningProcess process = get_running_process();
    print_invocation(process.invocation);
}
