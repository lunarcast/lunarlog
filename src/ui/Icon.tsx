interface IconProps {
  children: string;
  onClick?: () => void;
}

export const Icon = ({ children, onClick }: IconProps) => {
  return (
    <span onClick={onClick} class="material-icons">
      {children}
    </span>
  );
};
