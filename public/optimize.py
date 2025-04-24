import os

def is_constant(expr):
    try:
        eval(expr)
        return True
    except:
        return False

def evaluate(expr):
    try:
        return str(eval(expr))
    except:
        return expr

def optimize_code(lines):
    optimized = []
    constants = {}
    copies = {}
    expressions = {}

    for line in lines:
        if "=" not in line:
            optimized.append(line)
            continue

        lhs, rhs = line.split("=")
        lhs = lhs.strip()
        rhs = rhs.strip()

        # 1. Constant folding
        if is_constant(rhs):
            val = evaluate(rhs)
            constants[lhs] = val
            optimized.append(f"{lhs} = {val}")
            continue

        # 2. Copy propagation
        if rhs.isidentifier():
            while rhs in copies:
                rhs = copies[rhs]
            copies[lhs] = rhs
            continue  # skip adding direct copies

        # 3. Replace variables in rhs using copies
        tokens = ""
        token = ""
        for ch in rhs:
            if ch.isalnum() or ch == "_":
                token += ch
            else:
                if token:
                    token = copies.get(token, token)
                    tokens += token
                    token = ""
                tokens += ch
        if token:
            token = copies.get(token, token)
            tokens += token
        new_rhs = tokens

        # 4. Common Subexpression Elimination (for full RHS or parts)
        # Break down into known subexpressions
        replaced_rhs = new_rhs
        for expr, var in expressions.items():
            if expr in replaced_rhs:
                replaced_rhs = replaced_rhs.replace(expr, var)

        expressions[new_rhs] = lhs
        optimized.append(f"{lhs} = {replaced_rhs}")
        copies[lhs] = lhs

    return optimized

def main():
    print("Working directory:", os.getcwd())

    try:
        with open("C:/Users/soham/OneDrive/Desktop/priyal/exp9/input.txt", "r") as f:
            lines = [line.strip() for line in f if line.strip()]
    except FileNotFoundError:
        print("Error: 'input.txt' not found.")
        return

    print("\n--- Input Code (Before Optimization) ---")
    for line in lines:
        print(line)

    optimized = optimize_code(lines)

    print("\n--- Output Code (After Optimization) ---")
    for line in optimized:
        print(line)

if __name__ == "__main__":
    main()
