import { ComponentChildren } from ".pnpm/preact@10.5.14/node_modules/preact";
import "../styles/components/input.scss";

// ========== Types
type InputTypes = { number: number; text: string };

interface InputProps<T extends keyof InputTypes> {
  value: InputTypes[T];
  type?: T;
  label: string;
  id?: string;
  setValue: (v: InputTypes[T]) => void;
}

interface ButtonProps {
  onClick(): void;
  children: ComponentChildren;
  disabled?: boolean;
}

// ========== Components
export const Input = <T extends keyof InputTypes = "text">(
  props: InputProps<T>
) => {
  return (
    <div className="input">
      <span className="input__label" for={props.id}>
        {props.label}
      </span>

      <input
        type={props.type}
        className="input__input"
        id={props.id}
        value={props.value}
        onInput={(e) => {
          return props.setValue((e as any).target.value);
        }}
      />
    </div>
  );
};

export const Button = (props: ButtonProps) => {
  return (
    <button
      onClick={props.onClick}
      className={["button", props.disabled && "button--disabled"].join(" ")}
    >
      {props.children}
    </button>
  );
};
